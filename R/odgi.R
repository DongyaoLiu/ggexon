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

.is_odgi_graph_file <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path) &&
    grepl("\\.og$", path, ignore.case = TRUE)
}

.read_odgi_alignment_file <- function(x, odgi = NULL, python = NULL) {
  if (.is_odgi_graph_file(x)) {
    return(odgi_node_table(og_file = x, odgi = odgi, python = python, read = TRUE))
  }
  .read_odgi_node_table(x)
}

.odgi_split_occurrence_field <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(trimws(as.character(x)))) {
    return(character())
  }

  values <- strsplit(as.character(x), ",", fixed = TRUE)[[1L]]
  values <- trimws(values)
  values <- values[nzchar(values)]
  values[!to_upper_ascii(values) %in% "NA"]
}

.recycle_odgi_occurrence_field <- function(values, n, field, label) {
  if (length(values) == n) {
    return(values)
  }
  if (length(values) == 1L && n > 1L) {
    return(rep(values, n))
  }

  stop(
    "ODGI node table has inconsistent occurrence counts for label '",
    label,
    "' column ",
    field,
    ".",
    call. = FALSE
  )
}

.odgi_label_occurrences <- function(tbl, label) {
  if (!is.data.frame(tbl)) {
    stop("`tbl` must be a data.frame.", call. = FALSE)
  }

  chromosome_col <- paste0(label, "_chromosome")
  strand_col <- paste0(label, "_strand")
  start_col <- paste0(label, "_absolute_start")
  end_col <- paste0(label, "_absolute_end")
  required_cols <- c(chromosome_col, strand_col, start_col, end_col)
  missing_cols <- required_cols[!required_cols %in% names(tbl)]
  if (length(missing_cols) > 0L) {
    stop(
      "ODGI node table is missing columns for label '",
      label,
      "': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  out <- lapply(seq_len(nrow(tbl)), function(i) {
    chrom <- .odgi_split_occurrence_field(tbl[[chromosome_col]][[i]])
    strand <- .odgi_split_occurrence_field(tbl[[strand_col]][[i]])
    start <- .odgi_split_occurrence_field(tbl[[start_col]][[i]])
    end <- .odgi_split_occurrence_field(tbl[[end_col]][[i]])

    # The chromosome column is path-level metadata written on every row, even
    # when this node is absent from that path. Actual node occurrences are
    # defined by strand/start/end, not by the chromosome label alone.
    n_occ <- max(length(strand), length(start), length(end))
    if (n_occ == 0L) {
      return(NULL)
    }

    chrom <- .recycle_odgi_occurrence_field(chrom, n_occ, chromosome_col, label)
    strand <- .recycle_odgi_occurrence_field(strand, n_occ, strand_col, label)
    start <- as.integer(.recycle_odgi_occurrence_field(start, n_occ, start_col, label))
    end <- as.integer(.recycle_odgi_occurrence_field(end, n_occ, end_col, label))

    data.frame(
      node_id = tbl$node_id[[i]],
      occurrence_id = seq_len(n_occ),
      chr = chrom,
      strand = strand,
      start = pmin(start, end),
      end = pmax(start, end),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(out)
}

.odgi_label_chr_lengths <- function(tbl, label) {
  occ <- .odgi_label_occurrences(tbl, label)
  if (nrow(occ) == 0L) {
    return(setNames(integer(), character()))
  }

  out <- stats::aggregate(end ~ chr, data = occ, FUN = max)
  stats::setNames(as.integer(out$end), out$chr)
}

.odgi_pairwise_name <- function(base_name, query_individual, target_individual) {
  paste(base_name, query_individual, target_individual, sep = "__")
}

.odgi_label_match_key <- function(x) {
  x <- as.character(x)
  x <- sub("[._-].*$", "", x)
  x <- gsub("[^A-Za-z0-9]", "", x)
  to_upper_ascii(x)
}

.infer_odgi_label_mapping <- function(tbl, individuals = NULL) {
  labels <- .odgi_alignment_labels(tbl)
  if (is.null(individuals)) {
    return(stats::setNames(labels, labels))
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
    resolved <- trimws(as.character(unname(individuals[labels])))
    if (any(is.na(resolved)) || any(!nzchar(resolved))) {
      stop("`individuals` cannot contain missing or empty names.", call. = FALSE)
    }
    if (anyDuplicated(resolved)) {
      stop("`individuals` must not contain duplicates.", call. = FALSE)
    }
    return(stats::setNames(labels, resolved))
  }

  individuals <- trimws(as.character(individuals))
  if (any(is.na(individuals)) || any(!nzchar(individuals))) {
    stop("`individuals` cannot contain missing or empty names.", call. = FALSE)
  }
  if (anyDuplicated(individuals)) {
    stop("`individuals` must not contain duplicates.", call. = FALSE)
  }

  mapping <- stats::setNames(rep(NA_character_, length(individuals)), individuals)

  exact <- intersect(individuals, labels)
  if (length(exact) > 0L) {
    mapping[exact] <- exact
  }

  remaining_inds <- names(mapping)[is.na(mapping)]
  remaining_labels <- setdiff(labels, unname(stats::na.omit(mapping)))

  if (length(remaining_inds) > 0L) {
    ind_keys <- .odgi_label_match_key(remaining_inds)
    label_keys <- .odgi_label_match_key(remaining_labels)

    for (i in seq_along(remaining_inds)) {
      label_hits <- remaining_labels[label_keys == ind_keys[[i]]]
      ind_hits <- remaining_inds[ind_keys == ind_keys[[i]]]
      if (length(label_hits) == 1L && length(ind_hits) == 1L) {
        mapping[[remaining_inds[[i]]]] <- label_hits[[1L]]
      }
    }
  }

  if (anyNA(mapping)) {
    unresolved <- names(mapping)[is.na(mapping)]
    stop(
      "Could not infer ODGI path-label mappings for: ",
      paste(unresolved, collapse = ", "),
      ". Supply explicit label mappings, for example `individuals = c(label = individual)`.",
      call. = FALSE
    )
  }

  mapping
}

.odgi_pairwise_label_lookup <- function(msa, tbl = NULL) {
  labels <- msa@metadata$odgi_labels %||% NULL
  if (is.null(labels)) {
    if (!is.null(tbl)) {
      return(.infer_odgi_label_mapping(tbl, individuals = msa@individuals))
    }
    return(stats::setNames(msa@individuals, msa@individuals))
  }
  labels
}

.resolve_odgi_species_order <- function(msa,
                                        selected_species = NULL,
                                        reference_species = NULL) {
  species_order <- if (is.null(selected_species)) {
    alignment_individuals(msa)
  } else {
    unique(as.character(selected_species))
  }
  species_order <- species_order[species_order %in% alignment_individuals(msa)]

  if (!is.null(reference_species) && reference_species %in% alignment_individuals(msa)) {
    species_order <- c(reference_species, setdiff(species_order, reference_species))
  }

  unique(species_order)
}

.odgi_pairwise_table_from_multi <- function(msa,
                                            query_individual,
                                            target_individual) {
  if (!methods::is(msa, "SynMultiAlignment")) {
    stop("`msa` must be a SynMultiAlignment object.", call. = FALSE)
  }
  if (!identical(alignment_format(msa), "odgi")) {
    stop("`msa` must have `format = \"odgi\"`.", call. = FALSE)
  }
  if (!query_individual %in% msa@individuals) {
    stop("Unknown ODGI query individual: ", query_individual, call. = FALSE)
  }
  if (!target_individual %in% msa@individuals) {
    stop("Unknown ODGI target individual: ", target_individual, call. = FALSE)
  }
  if (identical(query_individual, target_individual)) {
    stop("`query_individual` and `target_individual` must differ.", call. = FALSE)
  }

  tbl <- multiple_alignment_data(msa)
  labels <- .odgi_pairwise_label_lookup(msa, tbl = tbl)
  query_label <- unname(labels[[query_individual]])
  target_label <- unname(labels[[target_individual]])
  if (is.null(query_label) || is.null(target_label)) {
    stop(
      "ODGI alignment metadata does not define label mappings for both requested individuals.",
      call. = FALSE
    )
  }

  query_occ <- .odgi_label_occurrences(tbl, query_label)
  target_occ <- .odgi_label_occurrences(tbl, target_label)
  if (nrow(query_occ) == 0L || nrow(target_occ) == 0L) {
    return(data.frame())
  }

  query_occ <- query_occ[, c("node_id", "occurrence_id", "chr", "strand", "start", "end")]
  names(query_occ) <- c("node_id", "q_occurrence_id", "qchr", "qstrand", "qstart", "qend")
  target_occ <- target_occ[, c("node_id", "occurrence_id", "chr", "strand", "start", "end")]
  names(target_occ) <- c("node_id", "t_occurrence_id", "tchr", "tstrand", "tstart", "tend")

  pair_df <- merge(query_occ, target_occ, by = "node_id", all = FALSE, sort = FALSE)
  if (nrow(pair_df) == 0L) {
    return(data.frame())
  }

  seq_width <- nchar(as.character(tbl$sequence[match(pair_df$node_id, tbl$node_id)]))
  qlen_map <- .odgi_label_chr_lengths(tbl, query_label)
  tlen_map <- .odgi_label_chr_lengths(tbl, target_label)

  pair_df$qlen <- as.integer(qlen_map[pair_df$qchr])
  pair_df$tlen <- as.integer(tlen_map[pair_df$tchr])
  pair_df$strand <- ifelse(pair_df$qstrand == pair_df$tstrand, "+", "-")
  pair_df$nmatch <- as.integer(seq_width)
  pair_df$alen <- as.integer(seq_width)
  pair_df$mapq <- NA_integer_
  pair_df$qspecies <- query_individual
  pair_df$tspecies <- target_individual
  pair_df$track <- paste0(
    "link_",
    .odgi_pairwise_name(alignment_name(msa), query_individual, target_individual)
  )

  pair_df <- pair_df[, c(
    "qchr", "qlen", "qstart", "qend", "strand",
    "tchr", "tlen", "tstart", "tend", "nmatch", "alen", "mapq",
    "qspecies", "tspecies", "track"
  )]
  rownames(pair_df) <- NULL
  pair_df
}

.odgi_pairwise_alignments_from_multi <- function(msa,
                                                 species_order = NULL) {
  if (!methods::is(msa, "SynMultiAlignment")) {
    stop("`msa` must be a SynMultiAlignment object.", call. = FALSE)
  }
  if (!identical(alignment_format(msa), "odgi")) {
    stop("`msa` must have `format = \"odgi\"`.", call. = FALSE)
  }

  species_order <- .resolve_odgi_species_order(msa, selected_species = species_order)
  if (length(species_order) < 2L) {
    return(list())
  }

  out <- lapply(seq_len(length(species_order) - 1L), function(i) {
    odgi_pairwise_alignment(
      x = msa,
      query_individual = species_order[[i]],
      target_individual = species_order[[i + 1L]]
    )
  })
  stats::setNames(out, vapply(out, alignment_name, character(1)))
}

.odgi_species_window_from_nodes <- function(tbl, label, node_ids) {
  occ <- .odgi_label_occurrences(tbl, label)
  occ <- occ[occ$node_id %in% node_ids, , drop = FALSE]
  if (nrow(occ) == 0L) {
    return(NULL)
  }

  occ$width <- occ$end - occ$start + 1L
  chr_summary <- stats::aggregate(width ~ chr, data = occ, FUN = sum)
  best_chr <- chr_summary$chr[[which.max(chr_summary$width)]]
  chr_occ <- occ[occ$chr == best_chr, , drop = FALSE]

  list(
    chr = best_chr,
    start = as.integer(min(chr_occ$start)),
    end = as.integer(max(chr_occ$end))
  )
}

.odgi_alignment_windows_from_reference <- function(msa,
                                                   reference_species,
                                                   chr,
                                                   start,
                                                   end,
                                                   selected_species = NULL) {
  if (!methods::is(msa, "SynMultiAlignment")) {
    stop("`msa` must be a SynMultiAlignment object.", call. = FALSE)
  }
  if (!identical(alignment_format(msa), "odgi")) {
    stop("`msa` must have `format = \"odgi\"`.", call. = FALSE)
  }
  if (!reference_species %in% alignment_individuals(msa)) {
    stop("Unknown ODGI reference species: ", reference_species, call. = FALSE)
  }

  tbl <- multiple_alignment_data(msa)
  labels <- .odgi_pairwise_label_lookup(msa, tbl = tbl)
  species_order <- .resolve_odgi_species_order(
    msa,
    selected_species = selected_species,
    reference_species = reference_species
  )
  if (length(species_order) == 0L) {
    stop("No selected species are present in the ODGI alignment.", call. = FALSE)
  }

  ref_label <- unname(labels[[reference_species]])
  ref_occ <- .odgi_label_occurrences(tbl, ref_label)
  ref_chr <- .resolve_paf_seqname(chr, unique(as.character(ref_occ$chr)))
  ref_hits <- ref_occ[
    ref_occ$chr == ref_chr &
      ref_occ$start < max(start, end) &
      ref_occ$end > min(start, end),
    ,
    drop = FALSE
  ]
  if (nrow(ref_hits) == 0L) {
    stop(
      "No ODGI nodes overlap ", reference_species, ":", ref_chr, ":",
      min(start, end), "-", max(start, end), ".",
      call. = FALSE
    )
  }

  node_ids <- unique(ref_hits$node_id)
  windows <- list()
  windows[[reference_species]] <- data.frame(
    chr = ref_chr,
    start = as.integer(min(start, end)),
    end = as.integer(max(start, end)),
    stringsAsFactors = FALSE
  )

  for (species_name in setdiff(species_order, reference_species)) {
    species_window <- .odgi_species_window_from_nodes(
      tbl = tbl,
      label = unname(labels[[species_name]]),
      node_ids = node_ids
    )
    if (is.null(species_window)) {
      next
    }
    windows[[species_name]] <- data.frame(
      chr = species_window$chr,
      start = species_window$start,
      end = species_window$end,
      stringsAsFactors = FALSE
    )
  }

  list(
    windows = windows,
    species_order = species_order[species_order %in% names(windows)],
    node_ids = node_ids
  )
}

.read_odgi_pairwise_alignment <- function(x, odgi = NULL, python = NULL) {
  labels <- x@metadata$odgi_labels %||% stats::setNames(
    c(query_individual(x), target_individual(x)),
    c(query_individual(x), target_individual(x))
  )
  multi <- SynMultiAlignment(
    name = x@metadata$odgi_alignment %||% alignment_name(x),
    individuals = unique(c(query_individual(x), target_individual(x))),
    file = alignment_file(x),
    format = "odgi",
    data = if (!is.null(x@metadata$odgi_table)) x@metadata$odgi_table else NULL,
    metadata = list(odgi_labels = labels)
  )
  if (is.null(multi@data)) {
    multi@data <- multiple_alignment_data(multi, odgi = odgi, python = python)
  }
  .odgi_pairwise_table_from_multi(
    multi,
    query_individual = query_individual(x),
    target_individual = target_individual(x)
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
#' Accepts either an in-memory node table returned by [odgi_node_table()], a
#' path to a TSV written by the bundled helper, or a raw `.og` ODGI graph
#' path. File-backed inputs are converted to the node-table representation,
#' validated, and stored on a `SynMultiAlignment` with `format = "odgi"`.
#'
#' @param x A data frame, an ODGI node-table TSV path, or an `.og` graph path.
#' @param name Optional alignment label. Defaults to the file stem when `x` is a
#'   path, otherwise `"odgi-alignment"`.
#' @param individuals Optional character vector/list describing which
#'   `SynIndividual` identifiers correspond to the ODGI path labels. If named,
#'   the names must match the path labels in the table.
#' @param odgi Optional path to the `odgi` executable. Used when `x` is an
#'   `.og` graph file.
#' @param python Optional path to the Python interpreter. Used when `x` is an
#'   `.og` graph file.
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
                                 odgi = NULL,
                                 python = NULL,
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
    .read_odgi_alignment_file(x, odgi = odgi, python = python)
  } else if (is.data.frame(x)) {
    x
  } else {
    stop("`x` must be a data.frame, an ODGI node-table TSV path, or an `.og` graph path.", call. = FALSE)
  }

  label_mapping <- .infer_odgi_label_mapping(tbl, individuals = individuals)
  alignment_inds <- names(label_mapping)
  source_file <- if (!is.null(file)) {
    file
  } else if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    x
  } else {
    "<odgi-node-table>"
  }

  metadata$odgi_labels <- label_mapping

  SynMultiAlignment(
    name = .resolve_odgi_alignment_name(name = name, file = source_file),
    individuals = alignment_inds,
    file = source_file,
    format = "odgi",
    data = tbl,
    metadata = metadata
  )
}

#' Convert an ODGI multiple alignment into a pairwise alignment
#'
#' Builds a PAF-like pairwise link table for one selected pair of individuals
#' from an ODGI node table, raw `.og` graph, or [`SynMultiAlignment`] with
#' `format = "odgi"`. The returned object is a [`SynPairAlignment`] with
#' `format = "odgi"`, so it can be added directly to a [`SynSpecies`] object
#' and consumed by `geom_nuclink()`.
#'
#' @param x A [`SynMultiAlignment`] with `format = "odgi"`, an ODGI node-table
#'   `data.frame`, a path to an ODGI node-table TSV, or a raw `.og` graph path.
#' @param query_individual Query-side individual identifier.
#' @param target_individual Target-side individual identifier.
#' @param name Optional pairwise alignment label. Defaults to
#'   `"<alignment>__<query>__<target>"`.
#' @param individuals Optional individual mapping used when `x` is not already a
#'   `SynMultiAlignment`. If named, the names must match ODGI path labels.
#' @param odgi Optional path to the `odgi` executable. Used when `x` is an
#'   `.og` graph file.
#' @param python Optional path to the Python interpreter. Used when `x` is an
#'   `.og` graph file.
#' @param file Optional source file to store on the returned object.
#' @param metadata Optional metadata list.
#'
#' @return A [`SynPairAlignment`] object with `format = "odgi"` and a cached
#'   PAF-like pairwise table in its `data` slot.
#' @export
odgi_pairwise_alignment <- function(x,
                                    query_individual,
                                    target_individual,
                                    name = NULL,
                                    individuals = NULL,
                                    odgi = NULL,
                                    python = NULL,
                                    file = NULL,
                                    metadata = list()) {
  if (!is.list(metadata)) {
    stop("`metadata` must be a list.", call. = FALSE)
  }

  msa <- if (methods::is(x, "SynMultiAlignment")) {
    if (!identical(alignment_format(x), "odgi")) {
      stop("`x` must have `format = \"odgi\"`.", call. = FALSE)
    }
    if (is.null(x@data)) {
      x@data <- multiple_alignment_data(x, odgi = odgi, python = python)
    }
    x
  } else {
    odgi_multi_alignment(
      x = x,
      name = name %||% NULL,
      individuals = individuals,
      odgi = odgi,
      python = python,
      file = file,
      metadata = metadata
    )
  }

  pair_name <- name %||% .odgi_pairwise_name(
    alignment_name(msa),
    query_individual,
    target_individual
  )
  metadata <- utils::modifyList(
    metadata,
    list(
      odgi_alignment = alignment_name(msa),
      odgi_labels = .odgi_pairwise_label_lookup(msa),
      odgi_table = msa@data
    )
  )

  SynPairAlignment(
    name = pair_name,
    query_individual = query_individual,
    target_individual = target_individual,
    file = file %||% alignment_file(msa),
    format = "odgi",
    data = .odgi_pairwise_table_from_multi(
      msa,
      query_individual = query_individual,
      target_individual = target_individual
    ),
    metadata = metadata
  )
}

#' Access data stored on a multiple alignment
#'
#' Returns the cached parsed representation stored on a [`SynMultiAlignment`].
#' For alignments with `format = "odgi"`, the data can also be loaded lazily
#' from either a tab-delimited ODGI node-table file or a raw `.og` graph on
#' disk. When called on a [`SynSpecies`], `alignment` selects which stored
#' multiple alignment to read.
#'
#' @param x A [`SynMultiAlignment`] object or a [`SynSpecies`] containing one or
#'   more multiple alignments.
#' @param alignment Optional multiple-alignment name when `x` is a
#'   [`SynSpecies`]. If omitted and exactly one multiple alignment is stored,
#'   that alignment is used.
#' @param odgi Optional path to the `odgi` executable. Used when an ODGI
#'   alignment is backed by a raw `.og` graph.
#' @param python Optional path to the Python interpreter. Used when an ODGI
#'   alignment is backed by a raw `.og` graph.
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

setMethod("multiple_alignment_data", "SynMultiAlignment", function(x, ..., odgi = NULL, python = NULL) {
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

  .read_odgi_alignment_file(alignment_file(x), odgi = odgi, python = python)
})

setMethod("multiple_alignment_data", "SynSpecies", function(x, alignment = NULL, ..., odgi = NULL, python = NULL) {
  multi <- .resolve_multiple_alignment_arg(x = x, alignment = alignment)
  multiple_alignment_data(multi, ..., odgi = odgi, python = python)
})
