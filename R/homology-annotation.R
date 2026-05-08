#' HomologyAnnotation class
#'
#' `HomologyAnnotation` stores cross-species gene homology mappings derived
#' from BLAST results. It is a species-level annotation attached to
#' `SynSpecies` via the `homology_annotations` slot.
#'
#' Each object maps genes from one query species to genes in a reference
#' species (typically the best-annotated "center" species). The mapping can
#' be used by `geom_genelabel()` to display reference-species gene names on
#' query-species tracks.
#'
#' @slot reference_species Scalar name of the reference species (e.g.,
#'   `"C. elegans N2"`).
#' @slot query_species Scalar name of the query species whose genes map to the
#'   reference.
#' @slot homology_table A data frame with at minimum the columns `query_gene`
#'   and `reference_gene`.
#'
#' @section Prototype defaults:
#' * `annotation_scope = "species"`
#' * `lazy = TRUE`
#' * `loaded = TRUE`
#' * `reference_species = NA_character_`
#' * `query_species = NA_character_`
#' * `homology_table = data.frame(query_gene = character(), reference_gene = character())`
#'
#' @section Validity rules:
#' * `reference_species` and `query_species` must each be one non-empty
#'   character value and must differ.
#' * `homology_table` must be a data frame containing at least `query_gene` and
#'   `reference_gene` columns.
#'
#' @exportClass HomologyAnnotation
setClass(
  "HomologyAnnotation",
  contains = "SynSpeAnnotation",
  slots = c(
    reference_species = "character",
    query_species = "character",
    homology_table = "data.frame"
  ),
  prototype = list(
    annotation_scope = "species",
    lazy = TRUE,
    loaded = TRUE,
    source_file = "<homology>",
    reference_species = NA_character_,
    query_species = NA_character_,
    homology_table = data.frame(
      query_gene = character(),
      reference_gene = character(),
      stringsAsFactors = FALSE
    )
  ),
  validity = function(object) {
    problems <- character()

    if (length(object@reference_species) != 1L ||
        is.na(object@reference_species) ||
        !nzchar(object@reference_species)) {
      problems <- c(
        problems,
        "`reference_species` must be a single non-empty character value."
      )
    }
    if (length(object@query_species) != 1L ||
        is.na(object@query_species) ||
        !nzchar(object@query_species)) {
      problems <- c(
        problems,
        "`query_species` must be a single non-empty character value."
      )
    }
    if (!is.na(object@reference_species) && !is.na(object@query_species) &&
        identical(object@reference_species, object@query_species)) {
      problems <- c(
        problems,
        "`reference_species` and `query_species` must differ."
      )
    }
    if (!is.data.frame(object@homology_table)) {
      problems <- c(problems, "`homology_table` must be a data frame.")
    } else if (!all(c("query_gene", "reference_gene") %in%
                    colnames(object@homology_table))) {
      problems <- c(
        problems,
        "`homology_table` must contain `query_gene` and `reference_gene` columns."
      )
    }

    if (length(problems) == 0L) TRUE else problems
  }
)

#' Constructor for HomologyAnnotation
#'
#' @param name Short unique label for the homology annotation layer.
#' @param reference_species Name of the reference (center) species.
#' @param query_species Name of the query species.
#' @param homology_table A data frame with at minimum `query_gene` and
#'   `reference_gene` columns.
#' @param source_file Optional path to the source BLAST file.
#' @param metadata Optional metadata list.
#'
#' @return A `HomologyAnnotation` object.
#' @export
HomologyAnnotation <- function(name,
                               reference_species,
                               query_species,
                               homology_table,
                               source_file = "<homology>",
                               metadata = list()) {
  required_cols <- c("query_gene", "reference_gene")
  if (!is.data.frame(homology_table)) {
    stop("`homology_table` must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(required_cols, colnames(homology_table))
  if (length(missing_cols) > 0L) {
    stop(
      "`homology_table` must contain columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  homology_table <- as.data.frame(homology_table, stringsAsFactors = FALSE)
  homology_table$query_gene <- as.character(homology_table$query_gene)
  homology_table$reference_gene <- as.character(homology_table$reference_gene)

  keep <- !is.na(homology_table$query_gene) &
    nzchar(homology_table$query_gene) &
    !is.na(homology_table$reference_gene) &
    nzchar(homology_table$reference_gene)
  homology_table <- homology_table[keep, , drop = FALSE]
  homology_table <- unique(homology_table)
  rownames(homology_table) <- NULL

  new(
    "HomologyAnnotation",
    name = name,
    source_file = source_file,
    reference_species = reference_species,
    query_species = query_species,
    homology_table = homology_table,
    metadata = metadata
  )
}

#' Import homology from a BLAST outfmt 6 file
#'
#' Parses a BLAST tabular output file (outfmt 6) and creates a
#' `HomologyAnnotation` object mapping query-species genes to
#' reference-species genes. The best hit per query (highest bitscore) is kept.
#'
#' BLAST outfmt 6 columns are expected as:
#' `qseqid sseqid pident length mismatch gapopen qstart qend sstart send evalue bitscore`
#'
#' Query sequence IDs typically carry prefixes (e.g., `transcript:`) and
#' isoform suffixes (e.g., `.t1`). These are stripped by default so that the
#' resulting `query_gene` values match annotation gene IDs.
#'
#' @param blast_file Path to the BLAST outfmt 6 file.
#' @param reference_species Name of the reference (center) species.
#' @param query_species Name of the query species whose proteins were BLASTed.
#' @param name Optional label for the homology annotation. Defaults to the
#'   blast file stem.
#' @param strip_prefix Regular expression matching prefixes to strip from
#'   query IDs. Defaults to `"^(transcript:|cds:|gene:)"`.
#' @param strip_suffix Regular expression matching suffixes to strip from
#'   query IDs. Defaults to `"\\.t\\d+$"` (transcript isoform numbers).
#' @param metadata Optional metadata list.
#'
#' @return A `HomologyAnnotation` object.
#' @export
import_blast_homology <- function(blast_file,
                                  reference_species,
                                  query_species,
                                  name = NULL,
                                  strip_prefix = "^(transcript:|cds:|gene:)",
                                  strip_suffix = "\\.t\\d+$",
                                  metadata = list()) {
  if (!is.character(blast_file) || length(blast_file) != 1L ||
      is.na(blast_file) || !nzchar(blast_file)) {
    stop("`blast_file` must be a single non-empty character value.", call. = FALSE)
  }
  if (!file.exists(blast_file)) {
    stop("BLAST file does not exist: ", blast_file, call. = FALSE)
  }

  if (is.null(name)) {
    name <- tools::file_path_sans_ext(basename(blast_file))
  }

  lines <- readLines(blast_file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  lines <- lines[!grepl("^\\s*#", lines)]

  if (length(lines) == 0L) {
    stop("BLAST file is empty: ", blast_file, call. = FALSE)
  }

  fields <- strsplit(lines, "\t", fixed = TRUE)
  n_fields <- vapply(fields, length, integer(1L))
  expected_n <- 12L
  valid <- n_fields >= expected_n
  if (!any(valid)) {
    stop(
      "BLAST file does not appear to be in outfmt 6 format ",
      "(expected >= 12 tab-separated fields).",
      call. = FALSE
    )
  }
  fields <- fields[valid]

  blast_df <- data.frame(
    qseqid   = vapply(fields, `[[`, character(1L), 1L),
    sseqid   = vapply(fields, `[[`, character(1L), 2L),
    bitscore = as.numeric(vapply(fields, `[[`, character(1L), 12L)),
    stringsAsFactors = FALSE
  )

  blast_df$normalized_query <- .normalize_blast_query_id(
    blast_df$qseqid,
    strip_prefix = strip_prefix,
    strip_suffix = strip_suffix
  )

  if (anyDuplicated(blast_df$normalized_query)) {
    blast_df <- blast_df[order(blast_df$normalized_query, -blast_df$bitscore), ]
    blast_df <- blast_df[!duplicated(blast_df$normalized_query), ]
  }

  homology_table <- data.frame(
    query_gene = blast_df$normalized_query,
    reference_gene = blast_df$sseqid,
    stringsAsFactors = FALSE
  )

  HomologyAnnotation(
    name = name,
    reference_species = reference_species,
    query_species = query_species,
    homology_table = homology_table,
    source_file = blast_file,
    metadata = c(metadata, list(
      n_blast_lines = length(lines),
      n_valid_lines = sum(valid),
      n_unique_query = nrow(homology_table)
    ))
  )
}

#' Normalize BLAST query IDs to gene-level identifiers
#'
#' Strips common prefixes (e.g., `transcript:`, `cds:`) and transcript
#' isoform suffixes (e.g., `.t1`, `.t2`) so that BLAST query identifiers
#' can be matched against annotation gene IDs.
#'
#' @param x Character vector of query sequence identifiers.
#' @param strip_prefix Regular expression for prefixes to remove.
#' @param strip_suffix Regular expression for suffixes to remove.
#'
#' @return A character vector of normalized gene-level identifiers.
#' @keywords internal
.normalize_blast_query_id <- function(x,
                                      strip_prefix = "^(transcript:|cds:|gene:)",
                                      strip_suffix = "\\.t\\d+$") {
  x <- as.character(x)
  if (!is.null(strip_prefix) && nzchar(strip_prefix)) {
    x <- sub(strip_prefix, "", x, perl = TRUE)
  }
  if (!is.null(strip_suffix) && nzchar(strip_suffix)) {
    x <- sub(strip_suffix, "", x, perl = TRUE)
  }
  x <- trimws(x)
  x
}

#' Retrieve the homology table from a HomologyAnnotation
#'
#' @param x A `HomologyAnnotation` object.
#'
#' @return A data frame with `query_gene` and `reference_gene` columns.
#' @export
setGeneric("homology_table", function(x) standardGeneric("homology_table"))
setMethod("homology_table", "HomologyAnnotation", function(x) x@homology_table)

#' Retrieve the reference species from a HomologyAnnotation
#'
#' @param x A `HomologyAnnotation` object.
#'
#' @return A scalar character value.
#' @export
setGeneric("reference_species", function(x) standardGeneric("reference_species"))
setMethod("reference_species", "HomologyAnnotation", function(x) x@reference_species)

#' Retrieve the query species from a HomologyAnnotation
#'
#' @param x A `HomologyAnnotation` object.
#'
#' @return A scalar character value.
#' @export
setGeneric("query_species", function(x) standardGeneric("query_species"))
setMethod("query_species", "HomologyAnnotation", function(x) x@query_species)

#' Apply homology-based labels to a gene data frame
#'
#' Replaces the `label` column for rows whose `gene_id` (or `gene_name`)
#' matches a `query_gene` entry in the homology table with the corresponding
#' `reference_gene`.
#'
#' @param gene_df A data frame with at least `gene_id`, `gene_name`, and
#'   `label` columns, as produced by `syn_to_gene_df()`.
#' @param homology A `HomologyAnnotation` object.
#' @param id_columns Character vector of column names in `gene_df` to match
#'   against `query_gene`. Defaults to `c("gene_id", "gene_name")`.
#'
#' @return The input data frame with updated `label` values where homology
#'   mappings exist.
#' @keywords internal
.apply_homology_labels <- function(gene_df,
                                   homology,
                                   id_columns = c("gene_id", "gene_name")) {
  if (!is.data.frame(gene_df) || nrow(gene_df) == 0L) {
    return(gene_df)
  }
  if (!methods::is(homology, "HomologyAnnotation")) {
    return(gene_df)
  }

  ht <- homology_table(homology)
  if (nrow(ht) == 0L) {
    return(gene_df)
  }

  available_cols <- intersect(id_columns, colnames(gene_df))
  if (length(available_cols) == 0L) {
    return(gene_df)
  }

  lookup <- stats::setNames(ht$reference_gene, ht$query_gene)

  for (col in available_cols) {
    gene_values <- as.character(gene_df[[col]])
    matches <- gene_values %in% names(lookup)
    if (any(matches)) {
      gene_df$label[matches] <- lookup[gene_values[matches]]
    }
  }

  gene_df
}

#' Apply homology labels per track (auto-matching)
#'
#' Like `.apply_homology_labels()`, but accepts a named list of
#' `HomologyAnnotation` objects. For each unique track in `gene_df`, the
#' matching homology is found by `query_species` and applied only to that
#' track's rows.
#'
#' @param gene_df A data frame with at least `gene_id`, `gene_name`, `label`,
#'   and `track` columns.
#' @param homology_list A named list of `HomologyAnnotation` objects.
#'
#' @return The input data frame with per-track label updates.
#' @keywords internal
.apply_homology_labels_auto <- function(gene_df, homology_list) {
  if (!is.data.frame(gene_df) || nrow(gene_df) == 0L) {
    return(gene_df)
  }
  if (!is.list(homology_list) || length(homology_list) == 0L) {
    return(gene_df)
  }

  tracks <- unique(as.character(gene_df$track))
  for (track_name in tracks) {
    ha <- NULL
    for (h in homology_list) {
      if (methods::is(h, "HomologyAnnotation") &&
          identical(query_species(h), track_name)) {
        ha <- h
        break
      }
    }
    if (is.null(ha)) {
      next
    }

    track_rows <- gene_df$track == track_name
    if (!any(track_rows)) {
      next
    }

    track_df <- gene_df[track_rows, , drop = FALSE]
    track_df <- .apply_homology_labels(track_df, ha)
    gene_df[track_rows, ] <- track_df
  }

  gene_df
}

#' @export
setMethod("show", "HomologyAnnotation", function(object) {
  cat("An object of class \"HomologyAnnotation\"\n")
  cat("  name:", object@name, "\n")
  cat("  reference_species:", object@reference_species, "\n")
  cat("  query_species:", object@query_species, "\n")
  cat("  homology rows:", nrow(object@homology_table), "\n")
  cat("  source_file:", object@source_file, "\n")
})
