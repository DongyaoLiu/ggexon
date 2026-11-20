.allele_frequency_vcf <- function(x, annotation, name) {
  reference <- annotation@reference_individual
  individual <- individuals(x)[[reference]]
  vcf <- get_annotation(individual, name)
  if (!methods::is(vcf, "SynVCFAnnotation") || !identical(annotation_name(vcf), name)) {
    stop("Annotation '", name, "' on '", reference, "' must be a SynVCFAnnotation.",
         call. = FALSE)
  }
  vcf
}

.allele_frequency_source_signature <- function(annotation, extra_path = NA_character_) {
  annotation@plot_cache <- list()
  rlang::hash(list(
    source = annotation,
    files = lapply(c(source_file(annotation), extra_path), .snp_file_signature)
  ))
}

.allele_frequency_signatures <- function(snp, vcf, pair) {
  c(
    snps = .allele_frequency_source_signature(snp),
    vcf = .allele_frequency_source_signature(vcf, vcf@index_file),
    alignment = .allele_frequency_source_signature(pair)
  )
}

.allele_frequency_track <- function(pair) {
  tracks <- unique(as.character(pair@data$track))
  if (length(tracks) == 0L) {
    label <- alignment_name(pair)
    if (is.null(pair@data) && alignment_format(pair) == "odgi") {
      label <- .odgi_pairwise_name(
        pair@metadata$odgi_alignment %||% label, query_individual(pair), target_individual(pair)
      )
    }
    return(paste0("link_", label))
  }
  if (!.snp_scalar_string(tracks)) {
    stop("The pairwise alignment must identify one link track.", call. = FALSE)
  }
  tracks
}

.read_variant_frequency <- function(info, field) {
  if (is.na(info)) {
    return(NA_real_)
  }
  tokens <- strsplit(info, ";", fixed = TRUE)[[1L]]
  entries <- tokens[sub("=.*$", "", tokens) == field]
  if (length(entries) == 0L || identical(entries, paste0(field, "=."))) {
    return(NA_real_)
  }
  if (length(entries) != 1L || !startsWith(entries, paste0(field, "="))) {
    return(NaN)
  }
  value <- substring(entries, nchar(field) + 2L)
  if (!grepl("^[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$", value)) {
    return(NaN)
  }
  af <- as.numeric(value)
  if (!is.finite(af) || af < 0 || af > 1) NaN else af
}

.allele_frequency_match <- function(variants, snps, af_field) {
  variants$source_row <- seq_len(nrow(variants))
  variants$af <- vapply(variants$INFO, .read_variant_frequency, numeric(1), field = af_field)
  ref <- to_upper_ascii(variants$REF)
  alt <- to_upper_ascii(variants$ALT)
  dna <- c("A", "C", "G", "T")
  reason <- rep(NA_character_, nrow(variants))
  biallelic <- !grepl(",", alt, fixed = TRUE)
  is_snp <- ref %in% dna & alt %in% dna & ref != alt
  reason[!biallelic] <- "multiallelic"
  reason[biallelic & !is_snp] <- "non_snp"
  reason[is_snp & is.na(variants$af)] <- "missing_af"
  reason[is_snp & is.nan(variants$af)] <- "invalid_af"

  snp_ref <- to_upper_ascii(snps$reference_base)
  snp_query <- to_upper_ascii(snps$query_base)
  snp_rows <- snp_ref %in% dna & snp_query %in% dna & snp_ref != snp_query
  overlapping <- rep(FALSE, nrow(snps))
  for (column in intersect(c("reference_overlap", "query_overlap"), names(snps))) {
    overlapping <- overlapping | snps[[column]] > 0
  }

  snp_key <- paste(snps$reference_chr, sprintf("%.0f", snps$reference_pos), sep = "\t")
  vcf_key <- paste(variants$CHROM, sprintf("%.0f", variants$POS), sep = "\t")
  # Match coordinates first, then resolve alleles and ambiguity on those rows.
  groups <- split(seq_len(nrow(snps)), snp_key)
  candidates <- match(vcf_key, names(groups))
  hits <- rep(NA_integer_, nrow(variants))
  for (row in which(is.na(reason))) {
    if (is.na(candidates[row])) {
      reason[row] <- "unmapped"
      next
    }
    rows <- groups[[candidates[row]]]
    rows <- rows[snp_rows[rows]]
    matching <- rows[snp_ref[rows] == ref[row] & snp_query[rows] == alt[row]]
    if (length(rows) == 0L) {
      reason[row] <- "non_snp"
    } else if (length(matching) == 0L) {
      reason[row] <- "allele_mismatch"
    } else if (length(matching) > 1L || overlapping[matching]) {
      reason[row] <- "ambiguous_mapping"
    } else {
      hits[row] <- matching
    }
  }
  excluded <- variants[!is.na(reason), , drop = FALSE]
  excluded$reason <- reason[!is.na(reason)]
  list(
    variants = variants[is.na(reason), , drop = FALSE],
    excluded = excluded,
    snps = snps[hits[!is.na(hits)], , drop = FALSE]
  )
}

.query_allele_frequency_vcf <- function(vcf, chromosomes) {
  path <- source_file(vcf)
  if (.is_bcf_file(path)) {
    .abort_unsupported_bcf()
  }
  if (length(chromosomes) == 0L) {
    return(as.data.frame(.vcf_lines_to_dataframe(character())))
  }

  indexed <- grepl("[.]gz$", path, ignore.case = TRUE) && !is.na(vcf@index_file) &&
    nzchar(vcf@index_file) && file.exists(path) && file.exists(vcf@index_file)
  if (indexed) {
    tabix <- Rsamtools::TabixFile(path, index = vcf@index_file)
    chromosomes <- intersect(chromosomes, Rsamtools::seqnamesTabix(tabix))
    if (length(chromosomes) == 0L) {
      return(as.data.frame(.vcf_lines_to_dataframe(character())))
    }
    # Prepare from the current file regardless of compatibility caches.
    vcf@region_cache <- list()
    records <- lapply(chromosomes, function(chr) {
      # scanTabix limits the end of a queried interval to 2^29.
      as.data.frame(query_variants(vcf, chr, 1L, 536870912L))
    })
    return(do.call(rbind, records))
  }

  lines <- .read_delimited_annotation_lines(path, skip_pattern = "^#")
  chr_index <- match(sub("\t.*$", "", lines), chromosomes)
  lines <- lines[order(chr_index, na.last = NA, method = "radix")]
  records <- as.data.frame(.vcf_lines_to_dataframe(lines))
  records[which(records$POS >= 1L), , drop = FALSE]
}

#' Prepare population allele frequencies for a linkage panel
#'
#' @param x A [SynSpecies()] object.
#' @param vcf Name of the [SynVCFAnnotation()] attached to the SNP source's
#'   reference individual.
#' @param snps Name of a stored [SynSNPAnnotation()].
#' @param name Label for this preparation.
#' @param af_field VCF INFO field containing one alternate allele frequency.
#'
#' @details Queries VCF records on the SNP source's reference chromosomes.
#'   Biallelic SNPs with one frequency in `[0, 1]` must match a unique
#'   reference and alternate allele pair at the same reference coordinate.
#'   Excluded records are retained with one reason per record.
#'
#'   The SNP source owns the cached preparation. Changes to the source or
#'   alignment can invalidate it. If the imported SNP file changes, import
#'   and attach the SNP source again before preparing.
#' @return An updated `SynSpecies` object.
#' @export
prepare_allele_frequency <- function(x, vcf, snps, name = "allele_frequency", af_field = "AF") {
  if (!.snp_scalar_string(name) || !.snp_scalar_string(vcf) || !.snp_scalar_string(snps)) {
    stop("Supply single, non-empty `name`, `vcf`, and `snps` labels.", call. = FALSE)
  }
  if (!.snp_scalar_string(af_field) || grepl("[;=,[:space:]]", af_field)) {
    stop("Supply one VCF INFO field name as `af_field`.", call. = FALSE)
  }
  snp <- .get_snp_annotation(x, snps)
  pair <- .snp_alignment(x, snp)
  vcf_annotation <- .allele_frequency_vcf(x, snp, vcf)
  signatures <- .allele_frequency_signatures(snp, vcf_annotation, pair)
  if (length(snp@import_signature) &&
      !identical(snp@import_signature, .snp_file_signature(source_file(snp)))) {
    stop("The SNP file changed after import. Import and attach it again.", call. = FALSE)
  }

  snp_rows <- query_snps(snp)
  records <- .query_allele_frequency_vcf(vcf_annotation, unique(snp_rows$reference_chr))
  matched <- .allele_frequency_match(records, snp_rows, af_field)

  variants <- matched$variants
  n <- nrow(variants)
  reference_is_target <- identical(snp@reference_individual, target_individual(pair))
  if (reference_is_target) {
    target <- "reference"
    query <- "query"
  } else {
    target <- "query"
    query <- "reference"
  }
  variants$tspecies <- rep(target_individual(pair), n)
  variants$tchr <- matched$snps[[paste0(target, "_chr")]]
  variants$tstart <- variants$tend <- matched$snps[[paste0(target, "_pos")]]
  variants$qspecies <- rep(query_individual(pair), n)
  variants$qchr <- matched$snps[[paste0(query, "_chr")]]
  variants$qstart <- variants$qend <- matched$snps[[paste0(query, "_pos")]]
  variants$strand <- matched$snps$strand
  variants$track <- rep(.allele_frequency_track(pair), n)
  variants$group <- variants$source_row
  variants$reference_is_target <- rep(reference_is_target, n)

  counts <- table(matched$excluded$reason)
  summary <- data.frame(reason = c("matched", names(counts)), n = c(n, as.integer(counts)))
  payload <- list(variants = variants, excluded = matched$excluded, summary = summary)
  if (!identical(signatures, .allele_frequency_signatures(snp, vcf_annotation, pair))) {
    stop("A source changed during preparation. Run `prepare_allele_frequency()` again.",
         call. = FALSE)
  }

  sources <- snp_annotations(x)
  registry <- x@metadata[[".ggexon_allele_frequency"]]
  old_snps <- registry[[name]]$snps
  if (!is.null(old_snps) && !identical(old_snps, snps) &&
      old_snps %in% names(sources)) {
    sources[[old_snps]]@plot_cache[["allele_frequency"]][[name]] <- NULL
  }
  snp@plot_cache[["allele_frequency"]][[name]] <- payload
  sources[[snps]] <- snp
  x@metadata$ggexon_snp_annotations <- sources
  registry[[name]] <- list(
    snps = snps, vcf = vcf, alignment = snp@alignment, af_field = af_field,
    reference_individual = snp@reference_individual, query_individual = snp@query_individual,
    signatures = signatures, payload_hash = rlang::hash(payload)
  )
  x@metadata[[".ggexon_allele_frequency"]] <- registry
  if (length(counts) > 0L) {
    warning(
      "Excluded ", nrow(matched$excluded), " VCF rows on the SNP source's reference chromosomes (",
      paste0(names(counts), "=", as.integer(counts), collapse = ", "), ").",
      call. = FALSE
    )
  }
  x
}

.allele_frequency_entry <- function(x, name = NULL) {
  if (!methods::is(x, "SynSpecies")) {
    stop("Supply a SynSpecies object.", call. = FALSE)
  }
  registry <- x@metadata[[".ggexon_allele_frequency"]]
  if (length(registry) == 0L) {
    stop("No allele-frequency preparations. Run `prepare_allele_frequency()` first.",
         call. = FALSE)
  }
  if (is.null(name) && length(registry) == 1L) {
    name <- names(registry)[[1L]]
  }
  if (!.snp_scalar_string(name)) {
    stop("Supply an allele-frequency preparation name.", call. = FALSE)
  }
  entry <- registry[[name]]
  if (is.null(entry)) {
    stop("Unknown allele-frequency preparation: ", name, call. = FALSE)
  }

  payload <- tryCatch({
    snp <- .get_snp_annotation(x, entry$snps)
    pair <- .snp_alignment(x, snp)
    vcf <- .allele_frequency_vcf(x, snp, entry$vcf)
    cached <- snp@plot_cache[["allele_frequency"]][[name]]
    if (identical(entry$alignment, snp@alignment) &&
        identical(entry$reference_individual, snp@reference_individual) &&
        identical(entry$query_individual, snp@query_individual) &&
        identical(entry$signatures, .allele_frequency_signatures(snp, vcf, pair)) &&
        identical(entry$payload_hash, rlang::hash(cached))) {
      cached
    }
  }, error = function(e) NULL)
  if (is.null(payload)) {
    stop(
      "Allele-frequency preparation '", name, "' is stale or its cache is missing. ",
      "Run `prepare_allele_frequency()` again. ",
      "If the SNP file changed, import and attach it again first.",
      call. = FALSE
    )
  }
  c(payload, entry)
}

#' Inspect prepared allele frequencies and exclusions
#'
#' @param x A [SynSpecies()] object.
#' @param name Preparation name. Optional when exactly one preparation exists.
#' @param component One of `"variants"`, `"excluded"`, or `"summary"`.
#'
#' @details `source_row` is the one-based row number among VCF records
#'   queried on the SNP source's reference chromosomes, grouped in the
#'   chromosome order of the SNP source. Summary counts cover those same
#'   queried records. Target and query columns follow the stored alignment
#'   order. `reference_is_target` identifies the reference edge for AF.
#' @return A data frame of matched variants, excluded records with a `reason`
#'   column, or counts by reason, according to `component`.
#' @export
query_allele_frequency <- function(x, name = NULL, component = c("variants", "excluded", "summary")) {
  component <- match.arg(component)
  .allele_frequency_entry(x, name)[[component]]
}
