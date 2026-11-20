.snp_columns <- c(
  "reference_chr", "reference_pos", "reference_base",
  "query_chr", "query_pos", "query_base", "strand"
)

.empty_snps <- function() {
  data.frame(
    reference_chr = character(), reference_pos = numeric(),
    reference_base = character(), query_chr = character(),
    query_pos = numeric(), query_base = character(), strand = character()
  )
}

.snp_scalar_string <- function(x) {
  rlang::is_string(x) && nzchar(x)
}

.snp_whole_numbers <- function(x, minimum = 1) {
  is.numeric(x) && all(is.finite(x) & x >= minimum & x == floor(x))
}

#' SynSNPAnnotation class
#'
#' Stores imported single-base differences for a named pairwise alignment.
#' Query positions are in forward-genome coordinates even on reverse strands.
#' Both bases are in the orientation of the forward reference genome.
#'
#' @slot reference_individual Identifier for the reference sequence.
#' @slot query_individual Identifier for the query sequence.
#' @slot alignment Name of the stored pairwise alignment.
#' @slot snps Data frame of normalized differences.
#' @slot import_signature File signature captured when importing the table.
#' @exportClass SynSNPAnnotation
setClass(
  "SynSNPAnnotation",
  contains = "SynSpeAnnotation",
  slots = c(
    reference_individual = "character",
    query_individual = "character",
    alignment = "character",
    snps = "data.frame",
    import_signature = "list"
  ),
  prototype = list(
    reference_individual = NA_character_, query_individual = NA_character_,
    alignment = NA_character_, lazy = FALSE, loaded = TRUE,
    snps = .empty_snps(), import_signature = list()
  ),
  validity = function(object) {
    for (slot_name in c("reference_individual", "query_individual", "alignment")) {
      if (!.snp_scalar_string(methods::slot(object, slot_name))) {
        return(paste0("`", slot_name, "` must be a single non-empty string."))
      }
    }
    if (identical(object@reference_individual, object@query_individual)) {
      return("`reference_individual` and `query_individual` must differ.")
    }
    if (!.snp_scalar_string(object@source_file)) {
      return("`snp_file` must be a single non-empty string.")
    }

    rows <- object@snps
    if (!all(.snp_columns %in% names(rows))) {
      return(paste0("`snps` must contain: ", paste(.snp_columns, collapse = ", "), "."))
    }
    for (side in c("reference", "query")) {
      chr <- rows[[paste0(side, "_chr")]]
      pos <- rows[[paste0(side, "_pos")]]
      base <- rows[[paste0(side, "_base")]]
      if (!is.character(chr) || anyNA(chr) || any(!nzchar(chr))) {
        return(paste0(side, " chromosome names must be non-empty strings."))
      }
      if (!.snp_whole_numbers(pos)) {
        return(paste0(side, " positions must be positive whole numbers."))
      }
      if (!is.character(base) || anyNA(base) || any(nchar(base) != 1L)) {
        return(paste0(side, " bases must each contain one nucleotide or gap character."))
      }
      overlap <- paste0(side, "_overlap")
      if (overlap %in% names(rows) && !.snp_whole_numbers(rows[[overlap]], minimum = 0)) {
        return(paste0(side, " overlap counts must be non-negative whole numbers."))
      }
    }
    if (!is.character(rows$strand) || !all(rows$strand %in% c("+", "-"))) {
      return("`strand` must contain only '+' or '-'.")
    }
    TRUE
  }
)

.snp_file_signature <- function(path) {
  if (length(path) != 1L || is.na(path)) {
    return(list(path = path))
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  info <- file.info(path)
  list(path = path, size = info$size, mtime = info$mtime)
}

.read_show_snps <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("SNP file does not exist: ", path, call. = FALSE)
  }
  lines <- .read_delimited_annotation_lines(path)
  lines <- lines[nzchar(trimws(lines))]
  bad_layout <- function() {
    stop(
      "Unsupported MUMmer SNP table. Use `show-snps -ClrTHI alignment.delta` ",
      "or tabular NUCMER output with its column header.",
      call. = FALSE
    )
  }

  first_columns <- c("reference_pos", "reference_base", "query_base",
                     "query_pos", "buffer", "distance")
  last_columns <- c("reference_frame", "query_frame", "reference_chr", "query_chr")
  sequence_lengths <- c("reference_length", "query_length")
  overlaps <- c("reference_overlap", "query_overlap")
  columns <- c(first_columns, sequence_lengths, last_columns)
  if (length(lines) >= 2L && lines[2L] %in% c("NUCMER", "PROMER")) {
    if (lines[2L] == "PROMER" || length(lines) < 3L) {
      bad_layout()
    }
    optional <- c("[R]" = overlaps[1L], "[Q]" = overlaps[2L],
                  "[LEN R]" = sequence_lengths[1L], "[LEN Q]" = sequence_lengths[2L])
    header <- strsplit(lines[3L], "\t", fixed = TRUE)[[1L]]
    middle <- head(tail(header, -6L), -2L)
    if (!identical(head(header, 6L), c("[P1]", "[SUB]", "[SUB]", "[P2]", "[BUFF]", "[DIST]")) ||
        !identical(tail(header, 2L), c("[FRM]", "[TAGS]")) ||
        !identical(middle, names(optional)[names(optional) %in% middle]) ||
        (("[R]" %in% middle) != ("[Q]" %in% middle)) ||
        (("[LEN R]" %in% middle) != ("[LEN Q]" %in% middle))) {
      bad_layout()
    }
    columns <- c(first_columns, unname(optional[middle]), last_columns)
    lines <- lines[-seq_len(3L)]
  }
  if (length(lines) == 0L) {
    return(.empty_snps())
  }

  fields <- strsplit(lines, "\t", fixed = TRUE)
  if (!all(lengths(fields) == length(columns))) {
    bad_layout()
  }
  rows <- as.data.frame(do.call(rbind, fields), stringsAsFactors = FALSE)
  names(rows) <- columns
  numeric_columns <- intersect(
    c("reference_pos", "query_pos", "buffer", "distance", sequence_lengths, overlaps),
    columns
  )
  for (column in numeric_columns) {
    values <- rows[[column]]
    if (any(!grepl("^[0-9]+$", values))) {
      bad_layout()
    }
    rows[[column]] <- as.numeric(values)
  }
  if (!all(c(rows$reference_frame, rows$query_frame) %in% c("1", "-1"))) {
    bad_layout()
  }

  # show-snps emits both bases in alignment orientation and coordinates in
  # forward-genome space. Only the reference frame governs complementation.
  rows$strand <- ifelse(rows$reference_frame == rows$query_frame, "+", "-")
  reverse_ref <- rows$reference_frame == "-1"
  for (column in c("reference_base", "query_base")) {
    bases <- to_upper_ascii(rows[[column]])
    bases[reverse_ref] <- chartr("ACGTRYSWKMBDHVN", "TGCAYRSWMKVHDBN", bases[reverse_ref])
    rows[[column]] <- bases
  }
  rows[c(.snp_columns, intersect(overlaps, columns))]
}

#' Import a MUMmer SNP correspondence
#'
#' @param name Unique label for the SNP source.
#' @param snp_file Path to a tabular NUCMER `show-snps` file.
#' @param reference_individual,query_individual Individual IDs in MUMmer's
#'   reference and query order.
#' @param alignment Name of the corresponding stored [SynPairAlignment()].
#'   The stored target and query can use either order.
#' @param snps Optional normalized data frame with `reference_chr`,
#'   `reference_pos`, `reference_base`, `query_chr`, `query_pos`,
#'   `query_base`, and `strand`. Coordinates are one-based. Optional
#'   `reference_overlap` and `query_overlap` count ambiguous alignments.
#'   When supplied, the file path is recorded as provenance.
#' @param metadata Optional source metadata list.
#'
#' @details Headerless input must use the 12-column layout produced by
#'   `show-snps -ClrTHI alignment.delta`, with sequence lengths and without
#'   overlap counts or sequence context. Tabular NUCMER output with its
#'   standard column header can also identify the optional overlap counts
#'   and sequence lengths.
#' @return A `SynSNPAnnotation` object.
#' @export
SynSNPAnnotation <- function(name, snp_file, reference_individual, query_individual,
                             alignment, snps = NULL, metadata = list()) {
  if (!.snp_scalar_string(snp_file)) {
    stop("`snp_file` must be a single non-empty string.", call. = FALSE)
  }
  signature <- list()
  if (is.null(snps)) {
    signature <- .snp_file_signature(snp_file)
    snps <- .read_show_snps(snp_file)
    if (!identical(signature, .snp_file_signature(snp_file))) {
      stop("The SNP file changed during import. Import it again.", call. = FALSE)
    }
    snp_file <- signature$path
  }
  new(
    "SynSNPAnnotation", name = name, source_file = snp_file,
    reference_individual = reference_individual, query_individual = query_individual,
    alignment = alignment, snps = snps, import_signature = signature,
    metadata = metadata
  )
}

.snp_alignment <- function(x, annotation) {
  name <- annotation@alignment
  pair <- pairwise_alignments(x)[[name]]
  members <- c(annotation@reference_individual, annotation@query_individual)
  if (!methods::is(pair, "SynPairAlignment") || !identical(alignment_name(pair), name)) {
    stop("Unknown pairwise alignment: ", name, call. = FALSE)
  }
  if (!setequal(alignment_individuals(pair), members)) {
    stop("SNP reference and query must match the individuals in alignment '", name, "'.",
         call. = FALSE)
  }
  if (!all(members %in% individual_names(x))) {
    stop("Both SNP individuals must be attached to the SynSpecies.", call. = FALSE)
  }
  pair
}

#' Attach an SNP source to a species collection
#'
#' @param x A [SynSpecies()] object.
#' @param annotation A [SynSNPAnnotation()] object.
#' @return An updated `SynSpecies` object.
#' @export
add_snp_annotation <- function(x, annotation) {
  if (!methods::is(x, "SynSpecies") || !methods::is(annotation, "SynSNPAnnotation")) {
    stop("Supply a SynSpecies and a SynSNPAnnotation.", call. = FALSE)
  }
  validObject(annotation)
  .snp_alignment(x, annotation)
  annotation@plot_cache[["allele_frequency"]] <- NULL
  annotations <- snp_annotations(x)
  annotations[[annotation_name(annotation)]] <- annotation
  x@metadata$ggexon_snp_annotations <- annotations
  x
}

#' List attached SNP sources
#'
#' @param x A [SynSpecies()] object.
#' @return A named list of [SynSNPAnnotation()] objects.
#' @export
snp_annotations <- function(x) {
  if (!methods::is(x, "SynSpecies")) {
    stop("Supply a SynSpecies object.", call. = FALSE)
  }
  annotations <- x@metadata[["ggexon_snp_annotations"]]
  if (is.null(annotations)) list() else annotations
}

.get_snp_annotation <- function(x, name) {
  annotations <- snp_annotations(x)
  if (is.null(name) && length(annotations) == 1L) {
    name <- names(annotations)[[1L]]
  }
  if (!.snp_scalar_string(name)) {
    stop("Supply an SNP annotation name.", call. = FALSE)
  }
  annotation <- annotations[[name]]
  if (!methods::is(annotation, "SynSNPAnnotation") ||
      !identical(annotation_name(annotation), name)) {
    stop("Unknown SNP annotation: ", name, call. = FALSE)
  }
  annotation
}

#' Inspect SNP correspondence records
#'
#' @param x A [SynSNPAnnotation()] or [SynSpecies()] object.
#' @param name SNP annotation name for `SynSpecies`. Optional when there is
#'   exactly one source.
#' @param chr Optional reference chromosome name or names.
#' @param start,end Optional inclusive reference coordinate bounds.
#' @return A data frame of normalized SNP records.
#' @export
query_snps <- function(x, name = NULL, chr = NULL, start = NULL, end = NULL) {
  annotation <- if (methods::is(x, "SynSNPAnnotation")) x else .get_snp_annotation(x, name)
  rows <- annotation@snps
  if (!is.null(chr) && (!is.character(chr) || anyNA(chr) || any(!nzchar(chr)))) {
    stop("`chr` must contain non-empty chromosome names.", call. = FALSE)
  }
  for (bound in list(start, end)) {
    if (!is.null(bound) && (length(bound) != 1L || !.snp_whole_numbers(bound))) {
      stop("`start` and `end` must be positive whole numbers.", call. = FALSE)
    }
  }
  if (!is.null(start) && !is.null(end) && start > end) {
    stop("`start` must be less than or equal to `end`.", call. = FALSE)
  }
  keep <- rep(TRUE, nrow(rows))
  if (!is.null(chr)) keep <- keep & rows$reference_chr %in% chr
  if (!is.null(start)) keep <- keep & rows$reference_pos >= start
  if (!is.null(end)) keep <- keep & rows$reference_pos <= end
  rows[keep, , drop = FALSE]
}
