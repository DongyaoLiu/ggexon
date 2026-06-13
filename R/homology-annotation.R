#' HomologyAnnotation class
#'
#' `HomologyAnnotation` stores cross-species gene homology mappings derived
#' from BLAST results. It is a species-level annotation attached to
#' `SynSpecies` via the `homology_annotations` slot.
#'
#' Each object maps genes from one query species to genes in a reference
#' species (typically the best-annotated "center" species). The mapping is
#' automatically injected into all geom data frames (`geom_exon`,
#' `geom_gene`, `geom_genetag`, `geom_genelabel`) when the
#' `HomologyAnnotation` is attached to a `SynSpecies`. Two new columns
#' `reference_gene` and `reference_gene_name` become available for mapping
#' in ggplot2 aesthetics.
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
#' * `homology_table$query_gene` must be unique.
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
    } else {
      query_gene <- as.character(object@homology_table$query_gene)
      keep <- !is.na(query_gene) & nzchar(query_gene)
      if (any(duplicated(query_gene[keep]))) {
        problems <- c(
          problems,
          "`homology_table$query_gene` must be unique."
        )
      }
    }

    if (length(problems) == 0L) TRUE else problems
  }
)

.format_duplicated_query_gene_warning <- function(query_gene) {
  duplicated_queries <- query_gene[
    duplicated(query_gene) | duplicated(query_gene, fromLast = TRUE)
  ]
  duplicated_counts <- table(duplicated_queries)
  entries <- paste0(
    names(duplicated_counts),
    " (",
    as.integer(duplicated_counts),
    " rows)"
  )
  paste(
    "Duplicated `query_gene` values were found and only the first row was kept:",
    paste(entries, collapse = ", ")
  )
}

.normalize_homology_table <- function(homology_table,
                                      warn_duplicates = TRUE,
                                      require_reference_gene = TRUE) {
  if (!is.data.frame(homology_table)) {
    stop("`homology_table` must be a data frame.", call. = FALSE)
  }

  required_cols <- "query_gene"
  if (isTRUE(require_reference_gene)) {
    required_cols <- c(required_cols, "reference_gene")
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
  if ("reference_gene" %in% colnames(homology_table)) {
    homology_table$reference_gene <- as.character(homology_table$reference_gene)
  }

  keep <- !is.na(homology_table$query_gene) &
    nzchar(homology_table$query_gene)
  if (isTRUE(require_reference_gene)) {
    keep <- keep &
      !is.na(homology_table$reference_gene) &
      nzchar(homology_table$reference_gene)
  }
  homology_table <- homology_table[keep, , drop = FALSE]

  if (nrow(homology_table) > 0L) {
    duplicated_query <- duplicated(homology_table$query_gene)
    if (any(duplicated_query)) {
      if (isTRUE(warn_duplicates)) {
        warning(
          .format_duplicated_query_gene_warning(homology_table$query_gene),
          call. = FALSE
        )
      }
      homology_table <- homology_table[!duplicated_query, , drop = FALSE]
    }
  }

  rownames(homology_table) <- NULL
  homology_table
}

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
  homology_table <- .normalize_homology_table(homology_table)
  reference_species <- unname(as.character(reference_species))
  query_species <- unname(as.character(query_species))

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

.homology_same_species <- function(x, y) {
  x <- unname(as.character(x))
  y <- unname(as.character(y))
  length(x) == 1L && length(y) == 1L &&
    !is.na(x) && !is.na(y) &&
    identical(x, y)
}

#' Import homology from a BLAST outfmt 6 file
#'
#' Parses a BLAST tabular output file (outfmt 6) and creates a
#' `HomologyAnnotation` object mapping query-species genes to
#' reference-species genes. The best hit per query is selected by ranking
#' on one or more BLAST metrics.
#'
#' @param blast_file Path to the BLAST outfmt 6 file.
#' @param reference_species Name of the reference (center) species.
#' @param query_species Name of the query species whose proteins were
#'   BLASTed.
#' @param name Optional label for the homology annotation. Defaults to the
#'   blast file stem.
#' @param outfmt The BLAST `-outfmt` column specification as a single string,
#'   e.g. `"6 qseqid sseqid pident length mismatch gapopen qstart qend
#'   sstart send evalue bitscore"`. The leading `"6 "` is stripped; the
#'   remaining tokens become the column names of the parsed table. This must
#'   match the columns actually written by BLAST.
#' @param rank_by One or more column names used to rank hits before
#'   deduplication. For `"evalue"` the sort is ascending (lower is better);
#'   all other columns are descending (higher is better). When multiple
#'   columns are given the first is the primary key. Defaults to
#'   `"bitscore"`.
#' @param gene_id_map Optional file path to a WormBase-style gene ID mapping
#'   (e.g. `c_elegans.PRJNA13758.WS285.geneIDs.txt`) or a named character
#'   vector mapping locus tags to gene names. When supplied, the
#'   `reference_gene` column is translated from locus tags (e.g. `"B0250.1"`)
#'   to gene names (e.g. `"calf-1"`). Isoform suffixes are stripped before
#'   lookup when an exact match is not found.
#' @param strip_prefix Regular expression matching prefixes to strip from
#'   query IDs. Defaults to `"^(transcript:|cds:|gene:)"`.
#' @param strip_suffix Regular expression matching suffixes to strip from
#'   query IDs. Defaults to `"(\\.t\\d+|-T\\d+)$"` (transcript isoform numbers,
#'   covering both `.t1` and Funannotate-style `-T1` conventions).
#' @param metadata Optional metadata list.
#'
#' @details
#' The `outfmt` string is the exact argument passed to `blastp -outfmt`.
#' All columns declared in `outfmt` must be present in the file; extra
#' columns are ignored, and lines with fewer fields than declared are
#' discarded.
#'
#' The `rank_by` parameter controls which BLAST metric(s) determine the
#' best hit kept per query. Common choices:
#'
#' - `"bitscore"` — highest bitscore (default)
#' - `"pident"` — highest percent identity
#' - `"evalue"` — lowest e-value
#' - `c("pident", "evalue")` — highest identity, then lowest e-value on ties
#'
#' @return A `HomologyAnnotation` object.
#' @export
import_blast_homology <- function(blast_file,
                                  reference_species,
                                  query_species,
                                  name = NULL,
                                  outfmt = paste(
                                    "6 qseqid sseqid pident length mismatch",
                                    "gapopen qstart qend sstart send evalue",
                                    "bitscore"
                                  ),
                                  rank_by = "bitscore",
                                  gene_id_map = NULL,
                                  strip_prefix = "^(transcript:|cds:|gene:)",
                                  strip_suffix = "(\\.t\\d+|-T\\d+)$",
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

  col_names <- .parse_blast_outfmt(outfmt)
  required_cols <- c("qseqid", "sseqid")
  missing_required <- setdiff(required_cols, col_names)
  if (length(missing_required) > 0L) {
    stop(
      "`outfmt` must include at least 'qseqid' and 'sseqid'. Missing: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  missing_rank <- setdiff(rank_by, col_names)
  if (length(missing_rank) > 0L) {
    stop(
      "`rank_by` column(s) not declared in `outfmt`: ",
      paste(missing_rank, collapse = ", "),
      call. = FALSE
    )
  }

  blast_df <- .read_blast_tabular(blast_file, col_names = col_names)

  blast_df$normalized_query <- .normalize_blast_query_id(
    blast_df$qseqid,
    strip_prefix = strip_prefix,
    strip_suffix = strip_suffix
  )

  blast_df <- .rank_blast_hits(blast_df, rank_by = rank_by)
  blast_df <- blast_df[!duplicated(blast_df$normalized_query), ]

  homology_table <- data.frame(
    query_gene     = blast_df$normalized_query,
    reference_gene = blast_df$sseqid,
    stringsAsFactors = FALSE
  )

  if (!is.null(gene_id_map)) {
    id_map <- .resolve_gene_id_map(gene_id_map)
    homology_table$reference_gene <- .translate_locus_tags(
      homology_table$reference_gene,
      id_map = id_map
    )
  }

  HomologyAnnotation(
    name              = name,
    reference_species = reference_species,
    query_species     = query_species,
    homology_table    = homology_table,
    source_file       = blast_file,
    metadata = c(metadata, list(
      n_input_rows     = nrow(blast_df),
      n_unique_query   = nrow(homology_table),
      outfmt           = outfmt,
      rank_by          = rank_by
    ))
  )
}

#' Parse a BLAST outfmt string into column names
#'
#' Strips the leading `"6 "` (tabular format specifier) and splits the
#' remainder on whitespace.
#'
#' @param outfmt Character string as passed to `blastp -outfmt`.
#'
#' @return Character vector of column names.
#' @keywords internal
.parse_blast_outfmt <- function(outfmt) {
  if (!is.character(outfmt) || length(outfmt) != 1L ||
      is.na(outfmt) || !nzchar(outfmt)) {
    stop("`outfmt` must be a single non-empty character value.", call. = FALSE)
  }

  outfmt <- trimws(outfmt)
  if (!grepl("^6\\s", outfmt)) {
    stop(
      "`outfmt` must start with '6 ' for BLAST tabular format.",
      call. = FALSE
    )
  }

  tokens <- strsplit(sub("^6\\s+", "", outfmt), "\\s+")[[1L]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0L) {
    stop("No column names found after '6 ' in `outfmt`.", call. = FALSE)
  }

  tokens
}

#' Read a BLAST tabular file with named columns
#'
#' Reads a tab-separated BLAST outfmt 6 file, assigns column names from the
#' parsed `outfmt` specification, and coerces numeric columns
#' automatically.
#'
#' @param blast_file Path to the BLAST tabular file.
#' @param col_names Character vector of column names, as returned by
#'   `.parse_blast_outfmt()`.
#'
#' @return A data frame with named columns.
#' @keywords internal
.read_blast_tabular <- function(blast_file, col_names) {
  lines <- readLines(blast_file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  lines <- lines[!grepl("^\\s*#", lines)]

  if (length(lines) == 0L) {
    stop("BLAST file is empty: ", blast_file, call. = FALSE)
  }

  fields <- strsplit(lines, "\t", fixed = TRUE)
  n_expected <- length(col_names)
  valid <- vapply(fields, length, integer(1L)) >= n_expected

  if (!any(valid)) {
    stop(
      "BLAST file does not contain enough columns. ",
      "Expected at least ", n_expected, " (from outfmt), ",
      "but no line had that many tab-separated fields.",
      call. = FALSE
    )
  }

  fields <- fields[valid]

  mat <- matrix(NA_character_, nrow = length(fields), ncol = n_expected)
  for (i in seq_along(fields)) {
    row <- fields[[i]]
    mat[i, ] <- row[seq_len(n_expected)]
  }

  blast_df <- as.data.frame(mat, stringsAsFactors = FALSE)
  colnames(blast_df) <- col_names

  numeric_cols <- setdiff(col_names, c("qseqid", "sseqid"))
  for (col in numeric_cols) {
    blast_df[[col]] <- suppressWarnings(as.numeric(blast_df[[col]]))
  }

  blast_df
}

#' Rank BLAST hits for deduplication
#'
#' Sorts a BLAST data frame by one or more ranking columns so that the
#' best hit per query appears first. The sort direction for `"evalue"` is
#' ascending (lower is better); all other columns are sorted descending
#' (higher is better).
#'
#' @param blast_df A data frame with at minimum `normalized_query` and the
#'   columns named in `rank_by`.
#' @param rank_by Character vector of column names to sort by.
#'
#' @return The input data frame, sorted.
#' @keywords internal
.rank_blast_hits <- function(blast_df, rank_by = "bitscore") {
  if (!is.character(rank_by) || length(rank_by) == 0L) {
    stop("`rank_by` must be a non-empty character vector.", call. = FALSE)
  }

  missing <- setdiff(rank_by, colnames(blast_df))
  if (length(missing) > 0L) {
    stop(
      "`rank_by` column(s) not found in BLAST data: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  ascending_cols <- "evalue"

  order_args <- list()
  for (col in rank_by) {
    is_ascending <- col %in% ascending_cols
    x <- blast_df[[col]]
    x[is.na(x)] <- if (is_ascending) Inf else -Inf
    order_args[[length(order_args) + 1L]] <- if (is_ascending) x else -x
  }

  order_idx <- do.call(order, order_args)
  blast_df <- blast_df[order_idx, , drop = FALSE]
  rownames(blast_df) <- NULL
  blast_df
}

#' Resolve a gene ID map from a file path or named vector
#'
#' Accepts either a path to a WormBase-style gene ID mapping file (CSV with
#' columns `WBGeneID`, `gene_name`, and `locus_tag`) or a named character
#' vector. Returns a named lookup vector from stable aliases to `gene_name`.
#'
#' @param gene_id_map A file path or a named character vector.
#'
#' @return A named character vector.
#' @keywords internal
.resolve_gene_id_map <- function(gene_id_map) {
  if (is.character(gene_id_map) && length(gene_id_map) == 1L &&
      !is.na(gene_id_map) && nzchar(gene_id_map) && file.exists(gene_id_map)) {
    return(.read_wormbase_gene_ids(gene_id_map))
  }

  if (is.character(gene_id_map) && !is.null(names(gene_id_map))) {
    map <- as.character(gene_id_map)
    map <- map[!is.na(map) & nzchar(map)]
    map <- map[!is.na(names(map)) & nzchar(names(map))]
    return(map)
  }

  stop(
    "`gene_id_map` must be a path to a gene ID file or a named character vector.",
    call. = FALSE
  )
}

#' Read a WormBase gene ID mapping file
#'
#' Parses a file like `c_elegans.PRJNA13758.WS285.geneIDs.txt`. Expected
#' columns: tax_id, WBGeneID, gene_name, locus_tag, status, type. Returns a
#' named vector with both `WBGeneID → gene_name` and `locus_tag → gene_name`
#' aliases.
#'
#' @param path Path to the gene ID file.
#'
#' @return A named character vector.
#' @keywords internal
.read_wormbase_gene_ids <- function(path) {
  if (!file.exists(path)) {
    stop("Gene ID mapping file does not exist: ", path, call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  lines <- lines[!grepl("^\\s*#", lines)]

  if (length(lines) == 0L) {
    stop("Gene ID mapping file is empty: ", path, call. = FALSE)
  }

  fields <- strsplit(lines, ",", fixed = TRUE)
  valid <- vapply(fields, length, integer(1L)) >= 4L
  if (!any(valid)) {
    stop(
      "Gene ID mapping file does not have at least 4 comma-separated columns.",
      call. = FALSE
    )
  }
  fields <- fields[valid]

  wb_gene_ids <- trimws(vapply(fields, `[[`, character(1L), 2L))
  gene_names <- trimws(vapply(fields, `[[`, character(1L), 3L))
  locus_tags <- trimws(vapply(fields, `[[`, character(1L), 4L))

  aliases <- c(wb_gene_ids, locus_tags)
  alias_gene_names <- rep(gene_names, 2L)

  keep <- nzchar(aliases) & nzchar(alias_gene_names)
  aliases <- aliases[keep]
  alias_gene_names <- alias_gene_names[keep]

  dupes <- duplicated(aliases)
  if (any(dupes)) {
    aliases <- aliases[!dupes]
    alias_gene_names <- alias_gene_names[!dupes]
  }

  stats::setNames(alias_gene_names, aliases)
}

#' Translate locus tags to gene names
#'
#' Looks up each value in a named mapping vector. Tries the exact value
#' first; on miss, strips locus-tag isoform suffixes (trailing letter +
#' optional digits, e.g. `"B0250.18a"` → `"B0250.18"`) and retries.
#' Unmapped values are returned unchanged.
#'
#' @param x Character vector of locus tags.
#' @param id_map Named character vector (`locus_tag → gene_name`).
#'
#' @return Character vector of translated gene names.
#' @keywords internal
.translate_locus_tags <- function(x, id_map) {
  x <- as.character(x)
  out <- id_map[x]
  unmapped <- is.na(out)
  if (!any(unmapped)) {
    return(out)
  }

  stripped <- sub("[a-z]\\d*$", "", x[unmapped], perl = TRUE)
  retry <- id_map[stripped]
  fallback_hit <- !is.na(retry)
  out[unmapped][fallback_hit] <- retry[fallback_hit]

  still_missing <- is.na(out)
  out[still_missing] <- x[still_missing]
  unname(out)
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
                                      strip_suffix = "(\\.t\\d+|-T\\d+)$") {
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

#' Normalize a gene identifier for symmetric matching
#'
#' Applies the same normalization to both BLAST query IDs and annotation
#' feature IDs so they can be matched reliably. Normalization steps:
#' 1. Trim whitespace
#' 2. Strip common prefixes (gene:, transcript:, cds:, mRNA:)
#' 3. Strip transcript isoform suffixes (.t1 .t2 ...  -T1 -T2 ...)
#' 4. Strip locus-tag isoform letters (B0250.18a → B0250.18)
#'
#' @param x Character vector of gene identifiers.
#'
#' @return A normalized character vector.
#' @keywords internal
.normalize_gene_id <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- sub("^(gene:|transcript:|cds:|mRNA:)", "", x, perl = TRUE)
  x <- sub("^(gene-|rna-|transcript-|cds-|mRNA-)", "", x, perl = TRUE)
  x <- sub("(\\.t\\d+|-T\\d+)(\\.mrna)?$", "", x, perl = TRUE)
  x <- sub("\\.mrna$", "", x, perl = TRUE)
  # Strip locus-tag isoform letters only when preceded by a digit
  # (e.g. B0250.18a → B0250.18, but WBGene stays WBGene)
  x <- sub("(\\d)[a-z]\\d*$", "\\1", x, perl = TRUE)
  x <- trimws(x)
  x
}

#' Inject homology reference columns into a geom data frame
#'
#' Adds `reference_gene` and `reference_gene_name` columns by matching
#' annotation feature IDs against `HomologyAnnotation` query genes.
#' Matching is done per-track using symmetric normalization of both
#' annotation-side IDs (gene_id, gene_name, transcript_id, Parent, ID)
#' and homology-side `query_gene` values.
#'
#' When no homology is available for a track, or when a feature has no
#' match, both new columns fall back to the feature's original gene_name
#' or gene_id, while `homology_hit` and `homology_query_hit` record whether a
#' real query-side homology table match was found. Reference-track rows that
#' match visible reference genes are marked with `homology_reference_hit`.
#'
#' @param df A data frame produced by a `syn_*_df()` function.
#'   Must contain at least a `track` column and one or more of
#'   `gene_id`, `gene_name`, `transcript_id`, `Parent`, `ID`.
#' @param homology_list A named list of `HomologyAnnotation` objects,
#'   typically from `homology_annotations(synspecies)`.
#'
#' @return The input data frame with additional `reference_gene`,
#'   `reference_gene_name`, `homology_hit`, `homology_query_hit`,
#'   `homology_reference_hit`, and `is_homology_reference_track` columns.
#' @keywords internal
.inject_homology_columns <- function(df, homology_list) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(df)
  }
  if (!is.list(homology_list) || length(homology_list) == 0L) {
    return(df)
  }

  id_candidates <- c(
    "gene_id", "gene_name", "transcript_id", "Parent", "ID",
    "homology_query_aliases"
  )
  id_columns <- intersect(id_candidates, colnames(df))
  if (length(id_columns) == 0L) {
    return(df)
  }

  fallback <- .homology_display_fallback(df)
  fallback[is.na(fallback) | !nzchar(fallback)] <- NA_character_

  df$reference_gene <- fallback
  df$reference_gene_name <- fallback
  df$homology_hit <- FALSE
  df$homology_query_hit <- FALSE
  df$homology_reference_hit <- FALSE
  df$is_homology_reference_track <- FALSE

  tracks <- unique(as.character(df$track))

  for (h in homology_list) {
    if (!methods::is(h, "HomologyAnnotation")) {
      next
    }
    ref_track <- reference_species(h)
    ref_rows <- which(vapply(
      df$track,
      .homology_same_species,
      logical(1),
      y = ref_track
    ))
    if (length(ref_rows) == 0L) {
      next
    }

    df$is_homology_reference_track[ref_rows] <- TRUE

    ht <- homology_table(h)
    if (nrow(ht) == 0L) {
      next
    }

    ref_gene <- as.character(ht$reference_gene)
    keep_ref <- !is.na(ref_gene) & nzchar(ref_gene)
    ref_gene <- unique(ref_gene[keep_ref])
    if (length(ref_gene) == 0L) {
      next
    }

    matched <- .homology_match_reference_rows(
      df = df,
      rows = ref_rows,
      ref_gene = ref_gene,
      id_columns = id_columns
    )
    df$reference_gene[matched$row_mask] <- matched$reference_gene[matched$row_mask]
    df$reference_gene_name[matched$row_mask] <- matched$reference_gene[matched$row_mask]
    df$homology_reference_hit[matched$row_mask] <- TRUE
  }

  native_reference_names <- .homology_display_fallback(df)
  has_native_reference_name <- df$is_homology_reference_track &
    !is.na(native_reference_names) &
    nzchar(native_reference_names)
  df$reference_gene_name[has_native_reference_name] <-
    native_reference_names[has_native_reference_name]

  for (track_name in tracks) {
    ha <- NULL
    for (h in homology_list) {
      if (methods::is(h, "HomologyAnnotation") &&
          .homology_same_species(methods::slot(h, "query_species"), track_name)) {
        ha <- h
        break
      }
    }
    if (is.null(ha)) {
      next
    }

    ht <- homology_table(ha)
    if (nrow(ht) == 0L) {
      next
    }

    norm_query <- .normalize_gene_id(ht$query_gene)
    ref_gene <- as.character(ht$reference_gene)

    keep <- !duplicated(norm_query) & nzchar(norm_query)
    norm_query <- norm_query[keep]
    ref_gene <- ref_gene[keep]

    matched <- rep(FALSE, length(df$track))
    matched_values <- rep(NA_character_, length(df$track))

    for (col in id_columns) {
      ann_values <- .homology_split_alias_values(df[[col]])
      norm_ann <- lapply(ann_values, .normalize_gene_id)

      for (i_packed in which(df$track == track_name)) {
        i <- as.integer(i_packed)
        if (matched[[i]]) next

        ni <- norm_ann[[i]]
        ni <- ni[!is.na(ni) & nzchar(ni)]
        if (length(ni) == 0L) next

        hit <- match(ni, norm_query)
        hit <- hit[!is.na(hit)]
        if (length(hit) > 0L) {
          matched[[i]] <- TRUE
          matched_values[[i]] <- ref_gene[[hit[[1L]]]]
        }
      }
    }

    label_columns <- intersect(c("gene_name", "gene", "label", "Name"), colnames(df))
    if (length(label_columns) > 0L && any(!matched)) {
      norm_ref <- .normalize_gene_id(ref_gene)
      keep_ref <- !duplicated(norm_ref) & !is.na(norm_ref) & nzchar(norm_ref)
      norm_ref <- norm_ref[keep_ref]
      ref_gene_lookup <- ref_gene[keep_ref]

      for (col in label_columns) {
        ann_values <- as.character(df[[col]])
        norm_ann <- .normalize_gene_id(ann_values)

        for (i_packed in which(df$track == track_name)) {
          i <- as.integer(i_packed)
          if (matched[[i]]) next

          ni <- norm_ann[[i]]
          if (is.na(ni) || !nzchar(ni)) next

          hit <- match(ni, norm_ref)
          if (!is.na(hit)) {
            matched[[i]] <- TRUE
            matched_values[[i]] <- ref_gene_lookup[[hit]]
          }
        }
      }
    }

    df$reference_gene[matched] <- matched_values[matched]
    df$reference_gene_name[matched] <- matched_values[matched]
    df$homology_hit[matched] <- TRUE
    df$homology_query_hit[matched] <- TRUE
  }

  df
}

.homology_display_fallback <- function(df) {
  display_df <- df
  if ("gene_name" %in% colnames(display_df)) {
    gene_name <- as.character(display_df$gene_name)
    display <- .coalesce_character_cols(display_df, c("gene", "label", "Name", "ID"))
    artificial <- .homology_artificial_gene_name(gene_name, display)
    gene_name[artificial] <- NA_character_
    display_df$gene_name <- gene_name
  }
  .coalesce_character_cols(
    display_df,
    c("gene_name", "gene", "label", "gene_id", "Name", "ID")
  )
}

.homology_artificial_gene_name <- function(gene_name, display) {
  gene_name <- as.character(gene_name)
  display <- as.character(display)
  stripped <- sub("^(gene|mRNA|transcript|rna|cds)[:_-]?", "", gene_name, perl = TRUE)
  !is.na(gene_name) & nzchar(gene_name) &
    !is.na(display) & nzchar(display) &
    gene_name != display &
    stripped == display
}

.homology_match_reference_rows <- function(df, rows, ref_gene, id_columns) {
  row_mask <- rep(FALSE, nrow(df))
  matched_values <- rep(NA_character_, nrow(df))
  norm_ref <- .normalize_gene_id(ref_gene)
  keep_ref <- !duplicated(norm_ref) & !is.na(norm_ref) & nzchar(norm_ref)
  norm_ref <- norm_ref[keep_ref]
  ref_gene <- ref_gene[keep_ref]
  if (length(norm_ref) == 0L) {
    return(list(row_mask = row_mask, reference_gene = matched_values))
  }

  match_columns <- unique(c(
    id_columns,
    intersect(c("gene_name", "gene", "label", "Name", "gene_id", "ID"), colnames(df))
  ))
  if (length(match_columns) == 0L) {
    return(list(row_mask = row_mask, reference_gene = matched_values))
  }

  for (col in match_columns) {
    ann_values <- .homology_split_alias_values(df[[col]])
    norm_ann <- lapply(ann_values, .normalize_gene_id)

    for (i in rows) {
      if (row_mask[[i]]) next

      ni <- norm_ann[[i]]
      ni <- ni[!is.na(ni) & nzchar(ni)]
      if (length(ni) == 0L) next

      hit <- match(ni, norm_ref)
      hit <- hit[!is.na(hit)]
      if (length(hit) > 0L) {
        row_mask[[i]] <- TRUE
        matched_values[[i]] <- ref_gene[[hit[[1L]]]]
      }
    }
  }

  list(row_mask = row_mask, reference_gene = matched_values)
}

.homology_split_alias_values <- function(x) {
  values <- as.character(x)
  lapply(values, function(value) {
    if (is.na(value) || !nzchar(value)) {
      return(character())
    }
    out <- strsplit(value, "\r", fixed = TRUE)[[1L]]
    out <- trimws(out)
    out[!is.na(out) & nzchar(out)]
  })
}

.homology_recycle_column <- function(value, n, arg) {
  value_length <- length(value)
  if (value_length == n) {
    return(value)
  }
  if (value_length == 1L) {
    return(rep(value, n))
  }

  stop(
    "`", arg, "` must have length 1 or match the length of `query_gene`.",
    call. = FALSE
  )
}

.homology_rows_from_vectors <- function(query_gene,
                                        reference_gene = NULL,
                                        dots = list(),
                                        require_reference_gene = TRUE) {
  if (is.null(query_gene)) {
    stop("Supply `query_gene` or `data`.", call. = FALSE)
  }
  n <- length(query_gene)
  if (n == 0L) {
    stop("`query_gene` must contain at least one value.", call. = FALSE)
  }

  dot_names <- names(dots)
  if (length(dots) > 0L &&
      (is.null(dot_names) || any(is.na(dot_names)) || any(!nzchar(dot_names)))) {
    stop("Extra homology columns supplied in `...` must be named.", call. = FALSE)
  }

  cols <- list(query_gene = query_gene)
  if (!is.null(reference_gene)) {
    cols$reference_gene <- reference_gene
  } else if (isTRUE(require_reference_gene)) {
    stop("`reference_gene` must be supplied.", call. = FALSE)
  }
  cols <- c(cols, dots)

  for (col in names(cols)) {
    cols[[col]] <- .homology_recycle_column(cols[[col]], n, col)
  }

  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

.homology_edit_rows <- function(data = NULL,
                                query_gene = NULL,
                                reference_gene = NULL,
                                dots = list(),
                                require_reference_gene = TRUE,
                                require_update_columns = FALSE) {
  if (!is.null(data)) {
    if (!is.null(query_gene) || !is.null(reference_gene) || length(dots) > 0L) {
      stop(
        "`data` cannot be supplied with `query_gene`, `reference_gene`, or extra columns in `...`.",
        call. = FALSE
      )
    }
    rows <- data
  } else {
    rows <- .homology_rows_from_vectors(
      query_gene = query_gene,
      reference_gene = reference_gene,
      dots = dots,
      require_reference_gene = require_reference_gene
    )
  }

  rows <- .normalize_homology_table(
    rows,
    require_reference_gene = require_reference_gene
  )
  if (isTRUE(require_update_columns) &&
      length(setdiff(colnames(rows), "query_gene")) == 0L) {
    stop("Supply at least one homology column to replace.", call. = FALSE)
  }
  rows
}

.homology_align_columns <- function(df, columns) {
  for (col in setdiff(columns, colnames(df))) {
    df[[col]] <- rep(NA, nrow(df))
  }
  df[, columns, drop = FALSE]
}

.homology_bind_rows <- function(x, y) {
  columns <- union(colnames(x), colnames(y))
  x <- .homology_align_columns(x, columns)
  y <- .homology_align_columns(y, columns)
  out <- rbind(x, y)
  rownames(out) <- NULL
  out
}

.homology_format_keys <- function(x) {
  paste(unique(as.character(x)), collapse = ", ")
}

.homology_patch_rows <- function(current, updates, add_missing = FALSE) {
  current <- .normalize_homology_table(current, warn_duplicates = FALSE)
  updates <- .normalize_homology_table(
    updates,
    require_reference_gene = FALSE
  )
  if (nrow(updates) == 0L) {
    return(current)
  }

  update_cols <- setdiff(colnames(updates), "query_gene")
  if (length(update_cols) == 0L) {
    stop("Supply at least one homology column to replace.", call. = FALSE)
  }

  missing_keys <- setdiff(updates$query_gene, current$query_gene)
  if (length(missing_keys) > 0L && !isTRUE(add_missing)) {
    stop(
      "Cannot replace missing `query_gene`: ",
      .homology_format_keys(missing_keys),
      call. = FALSE
    )
  }
  if (length(missing_keys) > 0L) {
    missing_rows <- updates$query_gene %in% missing_keys
    if (!"reference_gene" %in% colnames(updates) ||
        any(is.na(updates$reference_gene[missing_rows]) |
            !nzchar(updates$reference_gene[missing_rows]))) {
      stop(
        "Cannot add missing homology rows without non-empty `reference_gene` values.",
        call. = FALSE
      )
    }
  }

  columns <- union(colnames(current), colnames(updates))
  current <- .homology_align_columns(current, columns)
  updates <- .homology_align_columns(updates, columns)

  for (i in seq_len(nrow(updates))) {
    row_key <- updates$query_gene[[i]]
    hit <- match(row_key, current$query_gene)
    if (is.na(hit)) {
      new_row <- as.data.frame(
        stats::setNames(rep(list(NA), length(columns)), columns),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      new_row[1L, colnames(updates)] <- updates[i, colnames(updates), drop = FALSE]
      current <- rbind(current, new_row)
      hit <- nrow(current)
    }
    current[hit, update_cols] <- updates[i, update_cols, drop = FALSE]
  }

  .normalize_homology_table(current, warn_duplicates = FALSE)
}

.record_homology_edit <- function(x, action, n) {
  if (n <= 0L) {
    return(x)
  }
  edits <- x@metadata$homology_edits
  if (!is.list(edits)) {
    edits <- list()
  }
  edits[[length(edits) + 1L]] <- list(
    action = action,
    n = as.integer(n),
    timestamp = Sys.time()
  )
  x@metadata$homology_edits <- edits
  x
}

#' Retrieve or replace the homology table from a HomologyAnnotation
#'
#' `homology_table()` returns the current table. `homology_table<-` replaces the
#' full table after applying the same normalization rules as
#' [HomologyAnnotation()]: `query_gene` and `reference_gene` are coerced to
#' character, incomplete rows are dropped, duplicated `query_gene` values warn,
#' and the first row for each duplicated query is kept.
#'
#' @param x A `HomologyAnnotation` object.
#'
#' @return A data frame with `query_gene`, `reference_gene`, and any extra
#'   homology metadata columns.
#' @export
setGeneric("homology_table", function(x) standardGeneric("homology_table"))
#' @rdname homology_table
setMethod("homology_table", "HomologyAnnotation", function(x) x@homology_table)

#' @param x A `HomologyAnnotation` object.
#' @param value A data frame with at least `query_gene` and `reference_gene`.
#'
#' @return The updated `HomologyAnnotation` object.
#' @rdname homology_table
#' @export
setGeneric("homology_table<-", function(x, value) {
  standardGeneric("homology_table<-")
})
#' @rdname homology_table
setReplaceMethod("homology_table", "HomologyAnnotation", function(x, value) {
  x@homology_table <- .normalize_homology_table(value)
  x <- .record_homology_edit(x, action = "replace_table", n = nrow(x@homology_table))
  validObject(x)
  x
})

#' Edit homology rows
#'
#' These S4 methods add, delete, or replace rows in a `HomologyAnnotation`
#' table. Methods for `SynSpecies` edit one attached homology annotation selected
#' by `name`, by `query_species`, or by omission when exactly one homology
#' annotation is attached.
#'
#' @param x A `HomologyAnnotation` or `SynSpecies` object.
#' @param data Optional data frame of rows. For `add_homology()` it must contain
#'   `query_gene` and `reference_gene`. For `replace_homology()` it must contain
#'   `query_gene` plus at least one column to update. For `delete_homology()`,
#'   only `query_gene` and optional `reference_gene` are used.
#' @param query_gene Query-side gene IDs.
#' @param reference_gene Reference-side gene IDs. In `delete_homology()` this is
#'   an optional guard: rows are deleted only when the current reference gene
#'   matches.
#' @param ... Extra homology table columns for `add_homology()` and
#'   `replace_homology()`.
#' @param overwrite For `add_homology()`, whether incoming rows for existing
#'   `query_gene` values should update those rows. When `FALSE`, existing rows
#'   are kept and a warning is emitted.
#' @param add_missing For `replace_homology()`, whether missing `query_gene`
#'   values should be added. Missing rows require non-empty `reference_gene`
#'   values.
#' @param missing For `delete_homology()`, behavior when a requested
#'   `query_gene` is absent.
#' @param name Optional homology annotation name when `x` is a `SynSpecies`.
#' @param query_species Optional query species selector when `x` is a
#'   `SynSpecies`.
#'
#' @return The updated object.
#' @name homology-crud
NULL

#' @rdname homology-crud
#' @export
setGeneric("add_homology", function(x, ...) standardGeneric("add_homology"))

#' @rdname homology-crud
#' @export
setGeneric("delete_homology", function(x, ...) standardGeneric("delete_homology"))

#' @rdname homology-crud
#' @export
setGeneric("replace_homology", function(x, ...) standardGeneric("replace_homology"))

#' @rdname homology-crud
setMethod("add_homology", "HomologyAnnotation", function(x,
                                                          data = NULL,
                                                          query_gene = NULL,
                                                          reference_gene = NULL,
                                                          ...,
                                                          overwrite = FALSE) {
  rows <- .homology_edit_rows(
    data = data,
    query_gene = query_gene,
    reference_gene = reference_gene,
    dots = list(...),
    require_reference_gene = TRUE
  )
  if (nrow(rows) == 0L) {
    return(x)
  }

  current <- homology_table(x)
  existing <- rows$query_gene %in% current$query_gene
  if (any(existing) && !isTRUE(overwrite)) {
    warning(
      "Existing `query_gene` values were kept because `overwrite = FALSE`: ",
      .homology_format_keys(rows$query_gene[existing]),
      call. = FALSE
    )
    rows <- rows[!existing, , drop = FALSE]
  }

  if (isTRUE(overwrite)) {
    x@homology_table <- .homology_patch_rows(
      current = current,
      updates = rows,
      add_missing = TRUE
    )
    x <- .record_homology_edit(x, action = "add", n = nrow(rows))
    validObject(x)
    return(x)
  }

  if (nrow(rows) == 0L) {
    return(x)
  }
  x@homology_table <- .normalize_homology_table(
    .homology_bind_rows(current, rows),
    warn_duplicates = FALSE
  )
  x <- .record_homology_edit(x, action = "add", n = nrow(rows))
  validObject(x)
  x
})

#' @rdname homology-crud
setMethod("replace_homology", "HomologyAnnotation", function(x,
                                                              data = NULL,
                                                              query_gene = NULL,
                                                              reference_gene = NULL,
                                                              ...,
                                                              add_missing = FALSE) {
  rows <- .homology_edit_rows(
    data = data,
    query_gene = query_gene,
    reference_gene = reference_gene,
    dots = list(...),
    require_reference_gene = FALSE,
    require_update_columns = TRUE
  )
  x@homology_table <- .homology_patch_rows(
    current = homology_table(x),
    updates = rows,
    add_missing = add_missing
  )
  x <- .record_homology_edit(x, action = "replace", n = nrow(rows))
  validObject(x)
  x
})

#' @rdname homology-crud
setMethod("delete_homology", "HomologyAnnotation", function(x,
                                                            data = NULL,
                                                            query_gene = NULL,
                                                            reference_gene = NULL,
                                                            ...,
                                                            missing = c("error", "warn", "ignore")) {
  if (length(list(...)) > 0L) {
    stop("`delete_homology()` does not accept extra columns in `...`.", call. = FALSE)
  }
  missing <- match.arg(missing)
  rows <- .homology_edit_rows(
    data = data,
    query_gene = query_gene,
    reference_gene = reference_gene,
    dots = list(),
    require_reference_gene = FALSE
  )
  rows <- rows[, intersect(colnames(rows), c("query_gene", "reference_gene")), drop = FALSE]
  current <- homology_table(x)

  absent <- setdiff(rows$query_gene, current$query_gene)
  if (length(absent) > 0L) {
    message <- paste0(
      "Cannot delete missing `query_gene`: ",
      .homology_format_keys(absent)
    )
    if (identical(missing, "error")) {
      stop(message, call. = FALSE)
    }
    if (identical(missing, "warn")) {
      warning(message, call. = FALSE)
    }
  }

  rows <- rows[rows$query_gene %in% current$query_gene, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(x)
  }

  if ("reference_gene" %in% colnames(rows)) {
    guarded <- !is.na(rows$reference_gene) & nzchar(rows$reference_gene)
    mismatched <- character()
    for (i in which(guarded)) {
      hit <- match(rows$query_gene[[i]], current$query_gene)
      if (!identical(current$reference_gene[[hit]], rows$reference_gene[[i]])) {
        mismatched <- c(mismatched, rows$query_gene[[i]])
      }
    }
    if (length(mismatched) > 0L) {
      stop(
        "Cannot delete rows whose `reference_gene` guard does not match: ",
        .homology_format_keys(mismatched),
        call. = FALSE
      )
    }
  }

  keep <- !current$query_gene %in% rows$query_gene
  x@homology_table <- current[keep, , drop = FALSE]
  rownames(x@homology_table) <- NULL
  x <- .record_homology_edit(x, action = "delete", n = sum(!keep))
  validObject(x)
  x
})

#' @rdname homology-crud
setMethod("add_homology", "ANY", function(x, ...) {
  stop("`add_homology()` expects a HomologyAnnotation or SynSpecies object.", call. = FALSE)
})

#' @rdname homology-crud
setMethod("replace_homology", "ANY", function(x, ...) {
  stop("`replace_homology()` expects a HomologyAnnotation or SynSpecies object.", call. = FALSE)
})

#' @rdname homology-crud
setMethod("delete_homology", "ANY", function(x, ...) {
  stop("`delete_homology()` expects a HomologyAnnotation or SynSpecies object.", call. = FALSE)
})

#' Retrieve the reference species from a HomologyAnnotation
#'
#' @param x A `HomologyAnnotation` object.
#'
#' @return A scalar character value.
#' @export
setGeneric("reference_species", function(x) standardGeneric("reference_species"))
#' @rdname reference_species
setMethod("reference_species", "HomologyAnnotation", function(x) x@reference_species)

#' Retrieve the query species from a HomologyAnnotation
#'
#' @param x A `HomologyAnnotation` object.
#'
#' @return A scalar character value.
#' @export
setGeneric("query_species", function(x) standardGeneric("query_species"))
#' @rdname query_species
setMethod("query_species", "HomologyAnnotation", function(x) x@query_species)

#' @export
#' @rdname ggexon-show
setMethod("show", "HomologyAnnotation", function(object) {
  cat("An object of class \"HomologyAnnotation\"\n")
  cat("  name:", object@name, "\n")
  cat("  reference_species:", object@reference_species, "\n")
  cat("  query_species:", object@query_species, "\n")
  cat("  homology rows:", nrow(object@homology_table), "\n")
  cat("  source_file:", object@source_file, "\n")
})
