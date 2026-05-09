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
#' columns `gene_name` and `locus_tag`) or a named character vector. Returns
#' a named lookup vector `locus_tag → gene_name`.
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
#' named vector `locus_tag → gene_name`.
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

  locus_tags <- trimws(vapply(fields, `[[`, character(1L), 4L))
  gene_names <- trimws(vapply(fields, `[[`, character(1L), 3L))

  keep <- nzchar(locus_tags) & nzchar(gene_names)
  locus_tags <- locus_tags[keep]
  gene_names <- gene_names[keep]

  dupes <- duplicated(locus_tags)
  if (any(dupes)) {
    locus_tags <- locus_tags[!dupes]
    gene_names <- gene_names[!dupes]
  }

  stats::setNames(gene_names, locus_tags)
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
  x <- sub("(\\.t\\d+|-T\\d+)$", "", x, perl = TRUE)
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
#' or gene_id.
#'
#' @param df A data frame produced by a `syn_*_df()` function.
#'   Must contain at least a `track` column and one or more of
#'   `gene_id`, `gene_name`, `transcript_id`, `Parent`, `ID`.
#' @param homology_list A named list of `HomologyAnnotation` objects,
#'   typically from `homology_annotations(synspecies)`.
#'
#' @return The input data frame with additional `reference_gene` and
#'   `reference_gene_name` columns.
#' @keywords internal
.inject_homology_columns <- function(df, homology_list) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(df)
  }
  if (!is.list(homology_list) || length(homology_list) == 0L) {
    return(df)
  }

  id_candidates <- c("gene_id", "gene_name", "transcript_id", "Parent", "ID")
  id_columns <- intersect(id_candidates, colnames(df))
  if (length(id_columns) == 0L) {
    return(df)
  }

  fallback <- if ("gene_name" %in% colnames(df)) {
    as.character(df$gene_name)
  } else if ("gene_id" %in% colnames(df)) {
    as.character(df$gene_id)
  } else {
    rep(NA_character_, nrow(df))
  }
  fallback[is.na(fallback) | !nzchar(fallback)] <- NA_character_

  df$reference_gene <- fallback
  df$reference_gene_name <- fallback

  tracks <- unique(as.character(df$track))

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
      ann_values <- as.character(df[[col]])
      norm_ann <- .normalize_gene_id(ann_values)

      for (i_packed in which(df$track == track_name)) {
        i <- as.integer(i_packed)
        if (matched[[i]]) next

        ni <- norm_ann[[i]]
        if (is.na(ni) || !nzchar(ni)) next

        hit <- which(norm_query == ni)
        if (length(hit) > 0L) {
          matched[[i]] <- TRUE
          matched_values[[i]] <- ref_gene[[hit[[1L]]]]
        }
      }
    }

    df$reference_gene[matched] <- matched_values[matched]
    df$reference_gene_name[matched] <- matched_values[matched]
  }

  df
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

#' @export
setMethod("show", "HomologyAnnotation", function(object) {
  cat("An object of class \"HomologyAnnotation\"\n")
  cat("  name:", object@name, "\n")
  cat("  reference_species:", object@reference_species, "\n")
  cat("  query_species:", object@query_species, "\n")
  cat("  homology rows:", nrow(object@homology_table), "\n")
  cat("  source_file:", object@source_file, "\n")
})
