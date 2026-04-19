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

.read_odgi_node_table <- function(x) {
  utils::read.delim(
    x,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.odgi_alignment_labels <- function(tbl) {
  if (!is.data.frame(tbl)) {
    stop("`x` must be a data.frame or a path to an ODGI node-table TSV.", call. = FALSE)
  }

  required_base_cols <- c("node_id", "sequence")
  missing_base <- setdiff(required_base_cols, colnames(tbl))
  if (length(missing_base) > 0L) {
    stop(
      "ODGI node table is missing required columns: ",
      paste(missing_base, collapse = ", "),
      call. = FALSE
    )
  }

  suffixes <- c("_chromosome", "_strand", "_absolute_start", "_absolute_end")
  chromosome_cols <- grep("_chromosome$", colnames(tbl), value = TRUE)
  if (length(chromosome_cols) < 2L) {
    stop(
      "ODGI node table must contain at least two path label groups ending in '_chromosome'.",
      call. = FALSE
    )
  }

  labels <- sub("_chromosome$", "", chromosome_cols)
  missing_cols <- unlist(lapply(labels, function(label) {
    expected <- paste0(label, suffixes)
    expected[!expected %in% colnames(tbl)]
  }), use.names = FALSE)
  if (length(missing_cols) > 0L) {
    stop(
      "ODGI node table is missing path-specific columns: ",
      paste(unique(missing_cols), collapse = ", "),
      call. = FALSE
    )
  }

  labels
}

.normalize_odgi_alignment_individuals <- function(labels, individuals = NULL) {
  if (is.null(individuals)) {
    return(labels)
  }

  if (is.list(individuals)) {
    individuals <- unlist(individuals, use.names = TRUE)
  }

  if (!is.character(individuals) || length(individuals) != length(labels)) {
    stop(
      "`individuals` must be NULL or a character vector/list with one entry per ODGI path label.",
      call. = FALSE
    )
  }

  if (!is.null(names(individuals)) && any(nzchar(names(individuals)))) {
    if (!setequal(names(individuals), labels)) {
      stop(
        "Named `individuals` must use the ODGI path labels as names: ",
        paste(labels, collapse = ", "),
        call. = FALSE
      )
    }
    individuals <- unname(individuals[labels])
  }

  individuals <- trimws(as.character(individuals))
  if (any(is.na(individuals)) || any(!nzchar(individuals))) {
    stop("`individuals` cannot contain missing or empty names.", call. = FALSE)
  }
  if (anyDuplicated(individuals)) {
    stop("`individuals` must not contain duplicates.", call. = FALSE)
  }

  individuals
}

.resolve_odgi_alignment_name <- function(name, file, default = "odgi-alignment") {
  if (!is.null(name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
      stop("`name` must be NULL or a single non-empty character value.", call. = FALSE)
    }
    return(name)
  }

  if (!is.null(file) && is.character(file) && length(file) == 1L && !is.na(file) && nzchar(file)) {
    base <- basename(file)
    if (grepl("\\.gz$", base, ignore.case = TRUE)) {
      base <- tools::file_path_sans_ext(base)
    }
    base <- tools::file_path_sans_ext(base)
    if (nzchar(base)) {
      return(base)
    }
  }

  default
}

#' Convert an ODGI node table into a `SynMultiAlignment`
#'
#' Accepts either an in-memory node table returned by [odgi_node_table()] or a
#' path to a TSV written by the bundled helper, validates the path-specific
#' column groups, and stores the parsed table on a `SynMultiAlignment` with
#' `format = "odgi"`.
#'
#' @param x A data frame or a TSV path produced by [odgi_node_table()].
#' @param name Optional alignment label. Defaults to the file stem when `x` is a
#'   path, otherwise `"odgi-alignment"`.
#' @param individuals Optional character vector/list describing which
#'   `SynIndividual` identifiers correspond to the ODGI path labels. If named,
#'   the names must match the path labels in the table.
#' @param file Optional source file to store on the returned object. Defaults to
#'   `x` when `x` is a path, otherwise `"<odgi-node-table>"`.
#' @param metadata Optional metadata list.
#'
#' @return A `SynMultiAlignment` object with `format = "odgi"` and the parsed
#'   table cached in its `data` slot.
#' @export
#'
#' @examples
#' \dontrun{
#' tbl <- odgi_node_table("graph.og")
#' msa <- odgi_multi_alignment(tbl, name = "graph-msa")
#'
#' msa2 <- odgi_multi_alignment(
#'   "graph.node_table.tsv",
#'   individuals = c(XZ1516 = "XZ1516", N2 = "N2")
#' )
#' }
odgi_multi_alignment <- function(x,
                                 name = NULL,
                                 individuals = NULL,
                                 file = NULL,
                                 metadata = list()) {
  if (!is.null(file) &&
      (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file))) {
    stop("`file` must be NULL or a single non-empty character value.", call. = FALSE)
  }
  if (!is.list(metadata)) {
    stop("`metadata` must be a list.", call. = FALSE)
  }

  tbl <- if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    if (!file.exists(x)) {
      stop("ODGI node table not found: ", x, call. = FALSE)
    }
    .read_odgi_node_table(x)
  } else if (is.data.frame(x)) {
    x
  } else {
    stop("`x` must be a data.frame or a path to an ODGI node-table TSV.", call. = FALSE)
  }

  labels <- .odgi_alignment_labels(tbl)
  alignment_inds <- .normalize_odgi_alignment_individuals(labels, individuals = individuals)
  source_file <- if (!is.null(file)) {
    file
  } else if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    x
  } else {
    "<odgi-node-table>"
  }

  metadata$odgi_labels <- stats::setNames(labels, alignment_inds)

  SynMultiAlignment(
    name = .resolve_odgi_alignment_name(name = name, file = source_file),
    individuals = alignment_inds,
    file = source_file,
    format = "odgi",
    data = tbl,
    metadata = metadata
  )
}

#' Access data stored on a multiple alignment
#'
#' Returns the cached parsed representation stored on a [`SynMultiAlignment`].
#' For alignments with `format = "odgi"`, the data can also be loaded lazily
#' from the tab-delimited ODGI node-table file on disk. When called on a
#' [`SynSpecies`], `alignment` selects which stored multiple alignment to read.
#'
#' @param x A [`SynMultiAlignment`] object or a [`SynSpecies`] containing one or
#'   more multiple alignments.
#' @param alignment Optional multiple-alignment name when `x` is a
#'   [`SynSpecies`]. If omitted and exactly one multiple alignment is stored,
#'   that alignment is used.
#' @param ... Reserved for future extensions.
#'
#' @return A data frame containing the parsed multiple-alignment data.
#' @export
setGeneric("multiple_alignment_data", function(x, ...) standardGeneric("multiple_alignment_data"))

.resolve_multiple_alignment_arg <- function(x, alignment = NULL) {
  multi_list <- multiple_alignments(x)
  if (length(multi_list) == 0L) {
    stop("The SynSpecies object does not contain any multiple alignments.", call. = FALSE)
  }

  if (is.null(alignment)) {
    if (length(multi_list) == 1L) {
      return(multi_list[[1L]])
    }
    stop(
      "Supply `alignment` when multiple multiple-alignments are available: ",
      paste(names(multi_list), collapse = ", "),
      call. = FALSE
    )
  }

  if (!alignment %in% names(multi_list)) {
    stop(
      "`alignment` must be one of: ",
      paste(names(multi_list), collapse = ", "),
      call. = FALSE
    )
  }

  multi_list[[alignment]]
}

setMethod("multiple_alignment_data", "SynMultiAlignment", function(x, ...) {
  if (!is.null(x@data)) {
    return(x@data)
  }

  if (!identical(alignment_format(x), "odgi")) {
    stop(
      "Automatic parsing is currently implemented only for `SynMultiAlignment` objects with format = 'odgi'.",
      call. = FALSE
    )
  }
  if (!file.exists(alignment_file(x))) {
    stop("ODGI node-table file not found: ", alignment_file(x), call. = FALSE)
  }

  .read_odgi_node_table(alignment_file(x))
})

setMethod("multiple_alignment_data", "SynSpecies", function(x, alignment = NULL, ...) {
  multi <- .resolve_multiple_alignment_arg(x = x, alignment = alignment)
  multiple_alignment_data(multi, ...)
})
