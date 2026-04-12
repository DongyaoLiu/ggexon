#' SynIndividual class
#'
#' `SynIndividual` stores the per-individual data needed to build synteny plots.
#' The constructor only requires the source genome FASTA and annotation GFF/GTF
#' paths. Parsed annotations, nucleotide/protein sequences, and plotting caches
#' can be attached later through accessor methods.
#'
#' @slot id Scalar identifier for the species, genome, or plotting track.
#' @slot genome_file Path to the genome FASTA file.
#' @slot annotation_file Path to the corresponding GFF or GTF file.
#' @slot annotation_format One of `"gff"`, `"gtf"`, or `"auto"`.
#' @slot annotation Parsed annotation container used for plotting as a
#'   `GenomicRanges::GRanges` object.
#' @slot nucleotide_seq Nucleotide sequences extracted from the genome as a
#'   `Biostrings::DNAStringSet`.
#' @slot protein_seq Protein sequences translated from CDS annotations as a
#'   `Biostrings::AAStringSet`.
#' @slot seqinfo Sequence-level metadata such as chromosome names and lengths
#'   stored as a `GenomeInfoDb::Seqinfo` object.
#' @slot feature_index Fast lookup structure for genes, transcripts, or exons.
#' @slot metadata User or import metadata describing the individual.
#' @slot plot_cache Derived plotting tables cached for reuse.
#'
#' @exportClass SynIndividual
setClassUnion("NULLOrGRanges", c("NULL", "GRanges"))
setClassUnion("NULLOrSeqinfo", c("NULL", "Seqinfo"))
setClassUnion("NULLOrDNAStringSet", c("NULL", "DNAStringSet"))
setClassUnion("NULLOrAAStringSet", c("NULL", "AAStringSet"))

setClass(
  "SynIndividual",
  slots = c(
    id = "character",
    genome_file = "character",
    annotation_file = "character",
    annotation_format = "character",
    annotation = "NULLOrGRanges",
    nucleotide_seq = "NULLOrDNAStringSet",
    protein_seq = "NULLOrAAStringSet",
    seqinfo = "NULLOrSeqinfo",
    feature_index = "ANY",
    metadata = "list",
    plot_cache = "list"
  ),
  prototype = list(
    id = NA_character_,
    genome_file = NA_character_,
    annotation_file = NA_character_,
    annotation_format = "auto",
    annotation = NULL,
    nucleotide_seq = NULL,
    protein_seq = NULL,
    seqinfo = NULL,
    feature_index = NULL,
    metadata = list(),
    plot_cache = list()
  ),
  validity = function(object) {
    problems <- character()

    if (length(object@id) != 1L || is.na(object@id) || !nzchar(object@id)) {
      problems <- c(problems, "`id` must be a single non-empty character value.")
    }
    if (length(object@genome_file) != 1L || is.na(object@genome_file) ||
        !nzchar(object@genome_file)) {
      problems <- c(
        problems,
        "`genome_file` must be a single non-empty character value."
      )
    }
    if (length(object@annotation_file) != 1L || is.na(object@annotation_file) ||
        !nzchar(object@annotation_file)) {
      problems <- c(
        problems,
        "`annotation_file` must be a single non-empty character value."
      )
    }
    if (length(object@annotation_format) != 1L ||
        !(object@annotation_format %in% c("auto", "gff", "gtf"))) {
      problems <- c(
        problems,
        "`annotation_format` must be one of 'auto', 'gff', or 'gtf'."
      )
    }

    if (length(problems) == 0L) TRUE else problems
  }
)

#' Constructor for SynIndividual
#'
#' @param genome_file Path to the genome FASTA file.
#' @param annotation_file Path to the corresponding GFF or GTF file.
#' @param id Optional scalar identifier. Defaults to the FASTA stem.
#' @param annotation_format One of `"auto"`, `"gff"`, or `"gtf"`.
#' @param metadata Optional metadata list.
#'
#' @return A `SynIndividual` object with deferred slots left empty.
#' @export
SynIndividual <- function(genome_file,
                          annotation_file,
                          id = NULL,
                          annotation_format = c("auto", "gff", "gtf"),
                          metadata = list()) {
  annotation_format <- match.arg(annotation_format)

  check_syn_files(
    genome_file = genome_file,
    annotation_file = annotation_file
  )

  if (is.null(id)) {
    id <- tools::file_path_sans_ext(basename(genome_file))
  }

  new(
    "SynIndividual",
    id = id,
    genome_file = genome_file,
    annotation_file = annotation_file,
    annotation_format = annotation_format,
    metadata = metadata
  )
}

#' Check whether genome and annotation files match
#'
#' Validates that every sequence name in the first column of the annotation
#' file is present among the FASTA headers. FASTA names are compared using the
#' first token after `>`, which matches standard GFF/GTF `seqname` usage.
#'
#' @param genome_file Path to the genome FASTA file.
#' @param annotation_file Path to the corresponding GFF or GTF file.
#'
#' @return Invisibly returns `TRUE` when the files match.
#' @export
check_syn_files <- function(genome_file, annotation_file) {
  if (!is.character(genome_file) || length(genome_file) != 1L ||
      is.na(genome_file) || !nzchar(genome_file)) {
    stop("`genome_file` must be a single non-empty character value.", call. = FALSE)
  }
  if (!is.character(annotation_file) || length(annotation_file) != 1L ||
      is.na(annotation_file) || !nzchar(annotation_file)) {
    stop(
      "`annotation_file` must be a single non-empty character value.",
      call. = FALSE
    )
  }
  if (!file.exists(genome_file)) {
    stop("Genome FASTA file does not exist: ", genome_file, call. = FALSE)
  }
  if (!file.exists(annotation_file)) {
    stop("Annotation file does not exist: ", annotation_file, call. = FALSE)
  }

  fasta_headers <- .read_fasta_headers(genome_file)
  annotation_chr <- .read_annotation_seqnames(annotation_file)

  if (length(fasta_headers) == 0L) {
    stop("No FASTA headers were found in: ", genome_file, call. = FALSE)
  }
  if (length(annotation_chr) == 0L) {
    stop(
      "No sequence names were found in the first column of: ",
      annotation_file,
      call. = FALSE
    )
  }

  missing_chr <- setdiff(annotation_chr, fasta_headers)
  if (length(missing_chr) > 0L) {
    stop(
      paste0(
        "Annotation and genome files do not match. The following annotation ",
        "sequence names are missing from the FASTA headers: ",
        paste(utils::head(missing_chr, 10L), collapse = ", "),
        if (length(missing_chr) > 10L) " ..." else ""
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Load annotation into a SynIndividual object
#'
#' Imports the annotation file as a `GRanges` object and stores it in the
#' `annotation` slot. The imported ranges are lightly normalized so downstream
#' query and translation methods can use consistent metadata columns.
#'
#' @param x A `SynIndividual` object.
#'
#' @return An updated `SynIndividual` object.
#' @export
load_annotation <- function(x) {
  if (!methods::is(x, "SynIndividual")) {
    stop("`load_annotation()` expects a SynIndividual object.", call. = FALSE)
  }
  if (!is.null(annotation_data(x))) {
    return(x)
  }

  gr <- rtracklayer::import(annotation_file(x))
  gr <- .normalize_annotation(gr)

  annotation_data(x) <- gr
  seqinfo(x) <- GenomeInfoDb::seqinfo(gr)
  x
}

#' Query annotation features from a SynIndividual object
#'
#' @param x A `SynIndividual` object.
#' @param genes Optional character vector of gene names or gene identifiers.
#' @param transcripts Optional character vector of transcript identifiers.
#' @param chr Optional chromosome name.
#' @param start Optional start coordinate.
#' @param end Optional end coordinate.
#' @param feature_type Feature type to return. Defaults to `"CDS"`.
#' @param all Logical; when `TRUE`, return all matching `feature_type` records.
#'
#' @return A `GRanges` object containing the requested features.
#' @export
query_features <- function(x,
                           genes = NULL,
                           transcripts = NULL,
                           chr = NULL,
                           start = NULL,
                           end = NULL,
                           feature_type = "CDS",
                           all = FALSE) {
  if (!methods::is(x, "SynIndividual")) {
    stop("`query_features()` expects a SynIndividual object.", call. = FALSE)
  }

  x <- load_annotation(x)
  all_gr <- annotation_data(x)
  gr <- all_gr

  has_selector <- !is.null(genes) || !is.null(transcripts) || !is.null(chr) || all
  if (!has_selector) {
    stop(
      "Provide `genes`, `transcripts`, `chr`, or set `all = TRUE`.",
      call. = FALSE
    )
  }

  if (!is.null(genes)) {
    genes <- unique(as.character(genes))
    gene_match <- .match_annotation_values(
      all_gr,
      c("gene_name", "gene_id", "Name", "gene", "ID"),
      genes
    )
    gene_ids <- unique(.annotation_primary_ids(all_gr[gene_match]))

    transcript_ids <- unique(c(
      .annotation_transcript_ids(all_gr[gene_match]),
      .annotation_transcript_ids(
        all_gr[
          .match_annotation_values(
            all_gr,
            c("Parent"),
            gene_ids
          )
        ]
      )
    ))

    gene_filter <- .match_annotation_values(
      gr,
      c("gene_name", "gene_id", "Name", "gene", "ID"),
      genes
    )
    if (length(transcript_ids) > 0L) {
      gene_filter <- gene_filter | .match_annotation_values(
        gr,
        c("transcript_id", "Parent"),
        transcript_ids
      )
    }
    gr <- gr[gene_filter]
  }

  if (!is.null(transcripts)) {
    transcripts <- unique(as.character(transcripts))
    transcript_match <- .match_annotation_values(
      gr,
      c("transcript_id", "Parent", "transcript_name", "ID"),
      transcripts
    )
    gr <- gr[transcript_match]
  }

  if (!is.null(chr)) {
    chr <- as.character(chr)[1L]
    if (is.null(start)) {
      start <- 1L
    }
    if (is.null(end)) {
      chr_rows <- as.character(GenomeInfoDb::seqnames(gr)) == chr
      if (!any(chr_rows)) {
        gr <- gr[FALSE]
      } else {
        end <- max(IRanges::end(gr[chr_rows]))
      }
    }

    region <- GenomicRanges::GRanges(
      seqnames = chr,
      ranges = IRanges::IRanges(start = start, end = end)
    )
    gr <- gr[GenomicRanges::overlapsAny(gr, region)]
  }

  if (!is.null(feature_type)) {
    types <- as.character(S4Vectors::mcols(gr)$type)
    gr <- gr[
      !is.na(types) &
        base::tolower(types) == base::tolower(feature_type)
    ]
  }

  gr
}

#' Extract CDS nucleotide sequences
#'
#' @param x A `SynIndividual` object.
#' @param genes Optional character vector of gene names or identifiers.
#' @param transcripts Optional character vector of transcript identifiers.
#' @param chr Optional chromosome name.
#' @param start Optional start coordinate.
#' @param end Optional end coordinate.
#' @param all Logical; when `TRUE`, extract all CDS records.
#' @param store Logical; when `TRUE`, store the extracted sequences in
#'   `nucleotide_seq`.
#' @param append Logical; when `TRUE`, append new sequences to existing cached
#'   values by name.
#'
#' @return An updated `SynIndividual` object when `store = TRUE`, otherwise a
#'   `DNAStringSet`.
#' @export
extract_cds_seq <- function(x,
                            genes = NULL,
                            transcripts = NULL,
                            chr = NULL,
                            start = NULL,
                            end = NULL,
                            all = FALSE,
                            store = TRUE,
                            append = TRUE) {
  if (!methods::is(x, "SynIndividual")) {
    stop("`extract_cds_seq()` expects a SynIndividual object.", call. = FALSE)
  }

  x <- load_annotation(x)
  cds_gr <- query_features(
    x,
    genes = genes,
    transcripts = transcripts,
    chr = chr,
    start = start,
    end = end,
    feature_type = "CDS",
    all = all
  )

  cds_dna <- .extract_cds_sequences_from_gr(
    genome_file = genome_file(x),
    cds_gr = cds_gr
  )

  if (!store) {
    return(cds_dna)
  }

  existing <- nucleotide_seq(x)
  nucleotide_seq(x) <- .merge_string_sets(existing, cds_dna, append = append)
  x
}

#' Translate CDS sequences to proteins
#'
#' @param x A `SynIndividual` object.
#' @param genes Optional character vector of gene names or identifiers.
#' @param transcripts Optional character vector of transcript identifiers.
#' @param chr Optional chromosome name.
#' @param start Optional start coordinate.
#' @param end Optional end coordinate.
#' @param all Logical; when `TRUE`, translate all CDS records.
#' @param store Logical; when `TRUE`, store translated proteins in
#'   `protein_seq` and the CDS sequences in `nucleotide_seq`.
#' @param append Logical; when `TRUE`, append new sequences to existing cached
#'   values by name.
#' @param if.fuzzy.codon Passed to `Biostrings::translate()`.
#'
#' @return An updated `SynIndividual` object when `store = TRUE`, otherwise an
#'   `AAStringSet`.
#' @export
translate_protein <- function(x,
                              genes = NULL,
                              transcripts = NULL,
                              chr = NULL,
                              start = NULL,
                              end = NULL,
                              all = FALSE,
                              store = TRUE,
                              append = TRUE,
                              if.fuzzy.codon = "error") {
  cds_dna <- extract_cds_seq(
    x,
    genes = genes,
    transcripts = transcripts,
    chr = chr,
    start = start,
    end = end,
    all = all,
    store = FALSE
  )

  aa <- Biostrings::translate(cds_dna, if.fuzzy.codon = if.fuzzy.codon)
  names(aa) <- names(cds_dna)

  if (!store) {
    return(aa)
  }

  updated_x <- extract_cds_seq(
    x,
    genes = genes,
    transcripts = transcripts,
    chr = chr,
    start = start,
    end = end,
    all = all,
    store = TRUE,
    append = append
  )
  protein_seq(updated_x) <- .merge_string_sets(
    protein_seq(updated_x),
    aa,
    append = append
  )
  updated_x
}

#' Coerce and assign annotation data
#'
#' @param x A `SynIndividual` object.
#' @param value A `GenomicRanges::GRanges` object or `NULL`.
#'
#' @return The updated `SynIndividual` object.
#' @export
setGeneric("annotation_data<-", function(x, value) {
  standardGeneric("annotation_data<-")
})

.open_text_connection <- function(path) {
  if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }
}

.read_fasta_headers <- function(path) {
  con <- .open_text_connection(path)
  on.exit(close(con), add = TRUE)

  lines <- readLines(con, warn = FALSE)
  header_lines <- grep("^>", lines, value = TRUE)
  if (length(header_lines) == 0L) {
    return(character())
  }

  unique(sub("\\s.*$", "", substring(header_lines, 2L)))
}

.read_annotation_seqnames <- function(path) {
  con <- .open_text_connection(path)
  on.exit(close(con), add = TRUE)

  lines <- readLines(con, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines)]
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return(character())
  }

  fields <- strsplit(lines, "\t", fixed = TRUE)
  seqnames <- vapply(
    fields,
    function(x) {
      if (length(x) == 0L) "" else x[[1L]]
    },
    character(1)
  )
  seqnames <- trimws(seqnames)
  unique(seqnames[nzchar(seqnames)])
}

.normalize_annotation <- function(gr) {
  meta <- S4Vectors::mcols(gr)
  type <- as.character(meta$type)
  meta$type <- type

  raw_id <- if ("ID" %in% colnames(meta)) as.character(meta$ID) else rep(NA_character_, nrow(meta))
  raw_parent <- if ("Parent" %in% colnames(meta)) as.character(meta$Parent) else rep(NA_character_, nrow(meta))
  raw_name <- if ("Name" %in% colnames(meta)) as.character(meta$Name) else rep(NA_character_, nrow(meta))
  raw_gene_id <- if ("gene_id" %in% colnames(meta)) as.character(meta$gene_id) else rep(NA_character_, nrow(meta))
  raw_transcript_id <- if ("transcript_id" %in% colnames(meta)) {
    as.character(meta$transcript_id)
  } else {
    rep(NA_character_, nrow(meta))
  }

  gene_id <- raw_gene_id
  is_gene <- type == "gene"
  is_transcript <- type %in% c("mRNA", "transcript")
  is_child_feature <- type %in% c("CDS", "exon", "five_prime_UTR", "three_prime_UTR")

  gene_id[is_gene & (is.na(gene_id) | !nzchar(gene_id))] <- raw_id[is_gene & (is.na(gene_id) | !nzchar(gene_id))]
  gene_id[is_transcript & (is.na(gene_id) | !nzchar(gene_id))] <- raw_parent[is_transcript & (is.na(gene_id) | !nzchar(gene_id))]
  gene_id[is_child_feature & (is.na(gene_id) | !nzchar(gene_id))] <- NA_character_
  gene_id[is.na(gene_id) | !nzchar(gene_id)] <- .coalesce_character_cols(meta, c("gene", "gene_name", "Name"))[is.na(gene_id) | !nzchar(gene_id)]

  gene_name <- if ("gene_name" %in% colnames(meta)) as.character(meta$gene_name) else rep(NA_character_, nrow(meta))
  gene_name[is.na(gene_name) | !nzchar(gene_name)] <- raw_name[is.na(gene_name) | !nzchar(gene_name)]
  gene_name[is.na(gene_name) | !nzchar(gene_name)] <- gene_id[is.na(gene_name) | !nzchar(gene_name)]

  transcript_id <- raw_transcript_id
  transcript_id[is_transcript & (is.na(transcript_id) | !nzchar(transcript_id))] <- raw_id[is_transcript & (is.na(transcript_id) | !nzchar(transcript_id))]
  transcript_id[is_child_feature & (is.na(transcript_id) | !nzchar(transcript_id))] <- raw_parent[is_child_feature & (is.na(transcript_id) | !nzchar(transcript_id))]
  transcript_id[is_gene] <- NA_character_

  meta$gene_id <- gene_id
  meta$gene_name <- gene_name
  meta$transcript_id <- transcript_id
  meta$phase <- .coalesce_character_cols(meta, c("phase", "frame"))

  S4Vectors::mcols(gr) <- meta
  gr
}

.coalesce_character_cols <- function(meta, candidates) {
  values <- rep(NA_character_, nrow(meta))
  for (col in candidates) {
    if (!col %in% colnames(meta)) {
      next
    }
    current <- as.character(meta[[col]])
    use_current <- is.na(values) & !is.na(current) & nzchar(current)
    values[use_current] <- current[use_current]
  }
  values
}

.match_annotation_values <- function(gr, candidates, values) {
  values <- unique(as.character(values))
  if (length(values) == 0L) {
    return(rep(FALSE, length(gr)))
  }

  matches <- rep(FALSE, length(gr))
  meta <- S4Vectors::mcols(gr)
  for (col in candidates) {
    if (!col %in% colnames(meta)) {
      next
    }
    matches <- matches | as.character(meta[[col]]) %in% values
  }
  matches
}

.annotation_primary_ids <- function(gr) {
  .coalesce_character_cols(
    S4Vectors::mcols(gr),
    c("gene_id", "gene_name", "ID", "Name")
  )
}

.annotation_transcript_ids <- function(gr) {
  .coalesce_character_cols(
    S4Vectors::mcols(gr),
    c("transcript_id", "ID", "Parent")
  )
}

.extract_cds_sequences_from_gr <- function(genome_file, cds_gr) {
  if (length(cds_gr) == 0L) {
    return(Biostrings::DNAStringSet())
  }

  genome <- Biostrings::readDNAStringSet(filepath = genome_file)
  names(genome) <- sub("\\s.*$", "", names(genome))

  transcript_ids <- .coalesce_character_cols(
    S4Vectors::mcols(cds_gr),
    c("transcript_id", "Parent", "gene_id", "gene_name", "ID")
  )
  if (any(is.na(transcript_ids) | !nzchar(transcript_ids))) {
    stop("Every CDS record must have a transcript or gene identifier.", call. = FALSE)
  }

  split_index <- split(seq_along(cds_gr), transcript_ids)
  out <- vector("list", length(split_index))
  out_names <- names(split_index)

  for (i in seq_along(split_index)) {
    idx <- split_index[[i]]
    tx_gr <- cds_gr[idx]
    tx_name <- out_names[[i]]

    tx_seq <- Biostrings::DNAStringSet(.assemble_transcript_cds(tx_gr, genome))
    names(tx_seq) <- tx_name
    out[[i]] <- tx_seq
  }

  do.call(c, out)
}

.assemble_transcript_cds <- function(tx_gr, genome) {
  strand_value <- unique(as.character(BiocGenerics::strand(tx_gr)))
  strand_value <- strand_value[!is.na(strand_value)]
  if (length(strand_value) != 1L) {
    stop("CDS records for one transcript must share a single strand.", call. = FALSE)
  }

  seqname_value <- unique(as.character(GenomeInfoDb::seqnames(tx_gr)))
  if (length(seqname_value) != 1L) {
    stop("CDS records for one transcript must share a single seqname.", call. = FALSE)
  }
  seqname_value <- seqname_value[[1L]]
  if (!seqname_value %in% names(genome)) {
    stop("Sequence '", seqname_value, "' is missing from the genome FASTA.", call. = FALSE)
  }

  # Assemble CDS in transcript 5'->3' order: genomic low->high on '+' and
  # genomic high->low on '-', then reverse-complement each minus-strand segment
  # before concatenation so the final sequence is coding-space oriented.
  if (strand_value == "-") {
    order_idx <- order(IRanges::start(tx_gr), decreasing = TRUE)
  } else {
    order_idx <- order(IRanges::start(tx_gr))
  }
  tx_gr <- tx_gr[order_idx]

  tx_parts <- vector("list", length(tx_gr))
  chr_seq <- genome[[seqname_value]]

  for (i in seq_along(tx_gr)) {
    seg <- Biostrings::subseq(
      chr_seq,
      start = IRanges::start(tx_gr)[i],
      end = IRanges::end(tx_gr)[i]
    )
    if (strand_value == "-") {
      seg <- Biostrings::reverseComplement(seg)
    }
    tx_parts[[i]] <- seg
  }

  do.call(Biostrings::xscat, tx_parts)
}

.merge_string_sets <- function(existing, new_values, append = TRUE) {
  if (is.null(existing) || !append) {
    return(new_values)
  }

  if (!methods::is(existing, class(new_values)[1L])) {
    stop("Existing and new sequence caches must use the same class.", call. = FALSE)
  }

  existing_names <- names(existing)
  new_names <- names(new_values)
  if (is.null(existing_names) || any(!nzchar(existing_names))) {
    return(c(existing, new_values))
  }
  if (is.null(new_names) || any(!nzchar(new_names))) {
    return(c(existing, new_values))
  }

  keep_existing <- !existing_names %in% new_names
  c(existing[keep_existing], new_values)
}

#' @export
setMethod("show", "SynIndividual", function(object) {
  loaded <- c(
    annotation = !is.null(object@annotation),
    nucleotide_seq = !is.null(object@nucleotide_seq),
    protein_seq = !is.null(object@protein_seq),
    seqinfo = !is.null(object@seqinfo),
    feature_index = !is.null(object@feature_index)
  )

  cat("An object of class \"SynIndividual\"\n")
  cat("  id:", object@id, "\n")
  cat("  genome_file:", object@genome_file, "\n")
  cat("  annotation_file:", object@annotation_file, "\n")
  cat("  annotation_format:", object@annotation_format, "\n")
  cat("  loaded:", paste(names(loaded)[loaded], collapse = ", "), "\n")
})

setGeneric("syn_id", function(x) standardGeneric("syn_id"))
setMethod("syn_id", "SynIndividual", function(x) x@id)

setGeneric("genome_file", function(x) standardGeneric("genome_file"))
setMethod("genome_file", "SynIndividual", function(x) x@genome_file)

setGeneric("annotation_file", function(x) standardGeneric("annotation_file"))
setMethod("annotation_file", "SynIndividual", function(x) x@annotation_file)

setGeneric("annotation_format", function(x) standardGeneric("annotation_format"))
setMethod("annotation_format", "SynIndividual", function(x) x@annotation_format)

setGeneric("annotation_data", function(x) standardGeneric("annotation_data"))
setMethod("annotation_data", "SynIndividual", function(x) x@annotation)

setGeneric("nucleotide_seq", function(x) standardGeneric("nucleotide_seq"))
setMethod("nucleotide_seq", "SynIndividual", function(x) x@nucleotide_seq)

setGeneric("protein_seq", function(x) standardGeneric("protein_seq"))
setMethod("protein_seq", "SynIndividual", function(x) x@protein_seq)

setGeneric("seqinfo", function(x) standardGeneric("seqinfo"))
setMethod("seqinfo", "SynIndividual", function(x) x@seqinfo)

setGeneric("feature_index", function(x) standardGeneric("feature_index"))
setMethod("feature_index", "SynIndividual", function(x) x@feature_index)

setGeneric("syn_metadata", function(x) standardGeneric("syn_metadata"))
setMethod("syn_metadata", "SynIndividual", function(x) x@metadata)

setGeneric("plot_cache", function(x) standardGeneric("plot_cache"))
setMethod("plot_cache", "SynIndividual", function(x) x@plot_cache)

setReplaceMethod("annotation_data", "SynIndividual", function(x, value) {
  if (!is.null(value) && !methods::is(value, "GRanges")) {
    stop("`annotation_data<-` expects a GRanges object or NULL.", call. = FALSE)
  }
  x@annotation <- value
  validObject(x)
  x
})

setGeneric("nucleotide_seq<-", function(x, value) {
  standardGeneric("nucleotide_seq<-")
})
setReplaceMethod("nucleotide_seq", "SynIndividual", function(x, value) {
  if (!is.null(value) && !methods::is(value, "DNAStringSet")) {
    stop("`nucleotide_seq<-` expects a DNAStringSet object or NULL.", call. = FALSE)
  }
  x@nucleotide_seq <- value
  validObject(x)
  x
})

setGeneric("protein_seq<-", function(x, value) {
  standardGeneric("protein_seq<-")
})
setReplaceMethod("protein_seq", "SynIndividual", function(x, value) {
  if (!is.null(value) && !methods::is(value, "AAStringSet")) {
    stop("`protein_seq<-` expects an AAStringSet object or NULL.", call. = FALSE)
  }
  x@protein_seq <- value
  validObject(x)
  x
})

setGeneric("seqinfo<-", function(x, value) standardGeneric("seqinfo<-"))
setReplaceMethod("seqinfo", "SynIndividual", function(x, value) {
  if (!is.null(value) && !methods::is(value, "Seqinfo")) {
    stop("`seqinfo<-` expects a Seqinfo object or NULL.", call. = FALSE)
  }
  x@seqinfo <- value
  validObject(x)
  x
})

setGeneric("feature_index<-", function(x, value) {
  standardGeneric("feature_index<-")
})
setReplaceMethod("feature_index", "SynIndividual", function(x, value) {
  x@feature_index <- value
  validObject(x)
  x
})

setGeneric("syn_metadata<-", function(x, value) {
  standardGeneric("syn_metadata<-")
})
setReplaceMethod("syn_metadata", "SynIndividual", function(x, value) {
  if (!is.list(value)) {
    stop("`syn_metadata<-` expects a list.", call. = FALSE)
  }
  x@metadata <- value
  validObject(x)
  x
})

setGeneric("plot_cache<-", function(x, value) standardGeneric("plot_cache<-"))
setReplaceMethod("plot_cache", "SynIndividual", function(x, value) {
  if (!is.list(value)) {
    stop("`plot_cache<-` expects a list.", call. = FALSE)
  }
  x@plot_cache <- value
  validObject(x)
  x
})
