#' Locate the bundled ODGI node-table Python script
#'
#' Returns the installed path to ggexon's bundled `odgi_node_table.py` helper.
#' This is useful when you want to call the script directly from Python or a
#' shell while still relying on the copy shipped with the package.
#'
#' @return A scalar character path.
#' @export
odgi_node_table_script <- function() {
  script <- system.file("python", "odgi_node_table.py", package = "ggexon")
  if (!nzchar(script)) {
    stop("Bundled odgi_node_table.py could not be found.", call. = FALSE)
  }
  script
}

.resolve_python_binary <- function(python = NULL) {
  candidates <- if (is.null(python)) {
    c(Sys.which("python3"), Sys.which("python"))
  } else {
    python
  }

  candidates <- unique(as.character(candidates))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (candidate in candidates) {
    resolved <- if (grepl("[/\\\\]", candidate)) candidate else Sys.which(candidate)
    if (nzchar(resolved) && file.exists(resolved)) {
      return(resolved)
    }
  }

  stop(
    "A Python interpreter was not found. Set `python =` explicitly or add `python3` to PATH.",
    call. = FALSE
  )
}

.run_python_script <- function(python, script, args) {
  output <- system2(
    python,
    args = c(script, args),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(
      "The bundled odgi_node_table.py script failed:\n",
      paste(output, collapse = "\n"),
      call. = FALSE
    )
  }
  output
}

#' Build a node-by-node table from an ODGI graph
#'
#' Runs the bundled Python helper `odgi_node_table.py` against an `.og` graph
#' and returns either the generated TSV path or the parsed table.
#'
#' @param og_file Path to the input ODGI graph (`.og`).
#' @param output Optional output TSV path. Defaults to `<graph>.node_table.tsv`
#'   next to `og_file`.
#' @param odgi Optional path to the `odgi` executable. If omitted, the helper
#'   falls back to `ODGI_BIN` and then `odgi` on `PATH`.
#' @param python Optional path to the Python interpreter. Defaults to
#'   `python3`, then `python`, on `PATH`.
#' @param read Logical; if `TRUE`, read and return the generated TSV as a data
#'   frame. If `FALSE`, return the output path.
#'
#' @return A data frame when `read = TRUE`, otherwise the output file path.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- odgi_node_table("graph.og")
#'
#' path <- odgi_node_table(
#'   "graph.og",
#'   output = "graph.node_table.tsv",
#'   read = FALSE
#' )
#' }
odgi_node_table <- function(og_file,
                            output = NULL,
                            odgi = NULL,
                            python = NULL,
                            read = TRUE) {
  if (!is.character(og_file) || length(og_file) != 1L || is.na(og_file) || !nzchar(og_file)) {
    stop("`og_file` must be a single non-empty character value.", call. = FALSE)
  }
  if (!file.exists(og_file)) {
    stop("Input graph not found: ", og_file, call. = FALSE)
  }
  if (!is.null(output) &&
      (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output))) {
    stop("`output` must be NULL or a single non-empty character value.", call. = FALSE)
  }
  if (!is.logical(read) || length(read) != 1L || is.na(read)) {
    stop("`read` must be a single TRUE/FALSE value.", call. = FALSE)
  }

  python_bin <- .resolve_python_binary(python)
  script <- odgi_node_table_script()

  args <- c("--og", normalizePath(og_file))
  if (!is.null(output)) {
    args <- c(args, "--output", output)
  }
  if (!is.null(odgi)) {
    args <- c(args, "--odgi", odgi)
  }

  result <- .run_python_script(python_bin, script, args)
  result <- result[nzchar(trimws(result))]
  if (length(result) == 0L) {
    stop("The bundled odgi_node_table.py script did not report an output path.", call. = FALSE)
  }

  output_path <- tail(result, 1L)
  if (!file.exists(output_path)) {
    stop("Expected output file was not created: ", output_path, call. = FALSE)
  }

  if (!isTRUE(read)) {
    return(output_path)
  }

  utils::read.delim(
    output_path,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
