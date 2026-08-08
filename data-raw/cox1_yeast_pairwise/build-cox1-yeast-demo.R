# Generate the bundled fission/budding yeast cox1/COX1 pairwise demo.
#
# The script downloads PomBase mitochondrial gene models and sequence for
# Schizosaccharomyces pombe plus SGD S288C mitochondrial features and sequence
# for Saccharomyces cerevisiae, records exact source checksums, verifies the
# PomBase ortholog mapping, runs LASTZ on genomic windows spanning cox1/COX1,
# and writes plot-ready tables under inst/extdata.

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("data-raw/cox1_yeast_pairwise/build-cox1-yeast-demo.R", mustWork = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
if (!dir.exists(file.path(repo_root, "inst"))) {
  repo_root <- normalizePath(getwd(), mustWork = TRUE)
}

required_packages <- c("dplyr", "readr", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

out_dir <- file.path(repo_root, "inst", "extdata", "cox1_yeast_pairwise")
raw_dir <- file.path(repo_root, "data-raw", "cox1_yeast_pairwise", "downloads")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "annotations"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "sequences"), showWarnings = FALSE, recursive = TRUE)

lastz_bin <- Sys.getenv("LASTZ", unset = "/opt/homebrew/bin/lastz")
lastz_bin <- normalizePath(lastz_bin, mustWork = FALSE)

pombase_base <- "https://www.pombase.org/data"
sgd_base <- "https://downloads.yeastgenome.org"
pombase_gff3_url <- paste0(
  pombase_base,
  "/genome_sequence_and_features/gff3/Schizosaccharomyces_pombe_all_chromosomes.gff3.gz"
)
pombase_fasta_url <- paste0(
  pombase_base,
  "/genome_sequence_and_features/genome_sequence/Schizosaccharomyces_pombe_all_chromosomes.fa.gz"
)
pombase_ortholog_url <- paste0(
  pombase_base,
  "/orthologs/pombe-cerevisiae-orthologs.tsv"
)
sgd_features_url <- paste0(
  sgd_base,
  "/curation/chromosomal_feature/SGD_features.tab"
)
sgd_chrmt_fasta_url <- paste0(
  sgd_base,
  "/sequence/S288C_reference/chromosomes/fasta/chrmt.fsa"
)

window_padding <- 500L
transcript_y <- 0.2
lastz_min_len <- 40L
lastz_min_identity <- 50
lastz_identity_breaks <- c(50, 55, 60, 65, 70, Inf)
lastz_identity_labels <- c("50-55%", "55-60%", "60-65%", "65-70%", ">=70%")

species <- data.frame(
  species = c("fission_yeast", "budding_yeast"),
  display_name = c("Fission yeast", "Budding yeast"),
  scientific_name = c("Schizosaccharomyces pombe", "Saccharomyces cerevisiae S288C"),
  source_database = c("PomBase", "SGD"),
  gene_id = c("SPMIT.01", "Q0045"),
  gene_symbol = c("cox1", "COX1"),
  assembly = c("PomBase current reference", "S288C R64 mitochondrial reference"),
  seqname = c("mitochondrial", "17"),
  chr = c("mitochondrial", "chrmt"),
  fasta_seqname = c("mitochondrial", "chrmt"),
  stringsAsFactors = FALSE
)
species$species_order <- seq_len(nrow(species))

download_file_once <- function(url, dest) {
  if (!file.exists(dest) || file.info(dest)$size == 0L) {
    message("Downloading ", url)
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

read_text_gz <- function(path) {
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  readLines(con, warn = FALSE)
}

read_fasta <- function(path) {
  lines <- readLines(path, warn = FALSE)
  headers <- grep("^>", lines)
  if (length(headers) == 0L) {
    stop("No FASTA records found in ", path, call. = FALSE)
  }

  seqs <- character(length(headers))
  names <- character(length(headers))
  for (i in seq_along(headers)) {
    start <- headers[[i]] + 1L
    end <- if (i < length(headers)) headers[[i + 1L]] - 1L else length(lines)
    names[[i]] <- sub("^>([^ ]+).*", "\\1", lines[[headers[[i]]]])
    seqs[[i]] <- toupper(paste(lines[start:end], collapse = ""))
  }
  stats::setNames(seqs, names)
}

write_fasta <- function(name, sequence, path, width = 80L) {
  con <- file(path, open = "wt")
  on.exit(close(con), add = TRUE)
  writeLines(paste0(">", name), con)
  starts <- seq.int(1L, nchar(sequence), by = width)
  ends <- pmin(starts + width - 1L, nchar(sequence))
  writeLines(substring(sequence, starts, ends), con)
  invisible(path)
}

parse_gff3_attributes <- function(attributes, key) {
  pattern <- paste0("(?:^|;)", key, "=([^;]+)")
  match <- regexpr(pattern, attributes, perl = TRUE)
  out <- rep(NA_character_, length(attributes))
  hit <- match > 0L
  out[hit] <- sub(paste0(".*", pattern, ".*"), "\\1", attributes[hit], perl = TRUE)
  out[hit] <- utils::URLdecode(out[hit])
  out
}

read_pombase_gff3 <- function(path) {
  lines <- read_text_gz(path)
  lines <- lines[!grepl("^#", lines) & nzchar(lines)]
  gff <- utils::read.delim(
    textConnection(lines),
    header = FALSE,
    sep = "\t",
    quote = "",
    stringsAsFactors = FALSE
  )
  names(gff) <- c("seqid", "source", "type", "start", "end", "score", "strand", "phase", "attributes")
  gff$id <- parse_gff3_attributes(gff$attributes, "ID")
  gff$parent <- parse_gff3_attributes(gff$attributes, "Parent")
  gff$name <- parse_gff3_attributes(gff$attributes, "Name")
  gff
}

build_pombase_gene <- function(gff, species_row) {
  gene <- gff[gff$type == "gene" & gff$id == species_row$gene_id, , drop = FALSE]
  if (nrow(gene) != 1L) {
    stop("Expected exactly one PomBase gene for ", species_row$gene_id, call. = FALSE)
  }

  transcript <- gff[gff$type == "mRNA" & gff$parent == gene$id[[1L]], , drop = FALSE]
  if (nrow(transcript) != 1L) {
    stop("Expected exactly one PomBase mRNA for ", species_row$gene_id, call. = FALSE)
  }

  cds <- gff[gff$type == "CDS" & gff$parent == transcript$id[[1L]], , drop = FALSE]
  introns <- gff[gff$type == "intron" & gff$parent == transcript$id[[1L]], , drop = FALSE]
  cds <- cds[order(cds$start, cds$end), , drop = FALSE]
  introns <- introns[order(introns$start, introns$end), , drop = FALSE]

  gene_row <- data.frame(
    species = species_row$species,
    display_name = species_row$display_name,
    scientific_name = species_row$scientific_name,
    source_database = species_row$source_database,
    assembly = species_row$assembly,
    gene_id = gene$id,
    gene_symbol = gene$name %||% species_row$gene_symbol,
    seqname = gene$seqid,
    chr = species_row$chr,
    start = as.integer(gene$start),
    end = as.integer(gene$end),
    strand = ifelse(gene$strand == "-", -1L, 1L),
    length_bp = as.integer(gene$end) - as.integer(gene$start) + 1L,
    stringsAsFactors = FALSE
  )

  transcript_row <- data.frame(
    species = species_row$species,
    display_name = species_row$display_name,
    scientific_name = species_row$scientific_name,
    source_database = species_row$source_database,
    assembly = species_row$assembly,
    gene_id = gene$id,
    gene_symbol = gene$name %||% species_row$gene_symbol,
    seqname = transcript$seqid,
    chr = species_row$chr,
    transcript_id = transcript$id,
    transcript_name = gene$name %||% transcript$id,
    biotype = "protein_coding",
    is_canonical = 1L,
    transcript_start = as.integer(transcript$start),
    transcript_end = as.integer(transcript$end),
    strand = ifelse(transcript$strand == "-", -1L, 1L),
    exon_count = nrow(cds),
    intron_count = nrow(introns),
    stringsAsFactors = FALSE
  )

  exon_rows <- data.frame(
    species = species_row$species,
    gene_id = gene$id,
    gene_symbol = gene$name %||% species_row$gene_symbol,
    transcript_id = transcript$id,
    transcript_name = gene$name %||% transcript$id,
    transcript_biotype = "protein_coding",
    is_canonical = 1L,
    exon_rank = seq_len(nrow(cds)),
    exon_id = cds$id,
    seqname = cds$seqid,
    chr = species_row$chr,
    start = as.integer(cds$start),
    end = as.integer(cds$end),
    strand = ifelse(cds$strand == "-", -1L, 1L),
    source_database = species_row$source_database,
    stringsAsFactors = FALSE
  )

  intron_rows <- data.frame(
    species = species_row$species,
    gene_id = gene$id,
    gene_symbol = gene$name %||% species_row$gene_symbol,
    transcript_id = transcript$id,
    transcript_name = gene$name %||% transcript$id,
    intron_rank = seq_len(nrow(introns)),
    intron_id = introns$id,
    seqname = introns$seqid,
    chr = species_row$chr,
    start = as.integer(introns$start),
    end = as.integer(introns$end),
    strand = ifelse(introns$strand == "-", -1L, 1L),
    source_database = species_row$source_database,
    stringsAsFactors = FALSE
  )

  list(gene = gene_row, transcript = transcript_row, exons = exon_rows, introns = intron_rows)
}

read_sgd_features <- function(path) {
  sgd <- utils::read.delim(
    path,
    header = FALSE,
    sep = "\t",
    quote = "",
    stringsAsFactors = FALSE,
    fill = TRUE
  )
  names(sgd)[seq_len(16L)] <- paste0("V", seq_len(16L))
  sgd
}

build_sgd_gene <- function(sgd, species_row) {
  gene <- sgd[sgd$V2 == "ORF" & sgd$V4 == species_row$gene_id, , drop = FALSE]
  if (nrow(gene) != 1L) {
    stop("Expected exactly one SGD ORF for ", species_row$gene_id, call. = FALSE)
  }

  cds <- sgd[sgd$V2 == "CDS" & sgd$V7 == species_row$gene_id, , drop = FALSE]
  introns <- sgd[sgd$V2 == "intron" & sgd$V7 == species_row$gene_id, , drop = FALSE]
  cds$start_norm <- pmin(as.integer(cds$V10), as.integer(cds$V11))
  cds$end_norm <- pmax(as.integer(cds$V10), as.integer(cds$V11))
  introns$start_norm <- pmin(as.integer(introns$V10), as.integer(introns$V11))
  introns$end_norm <- pmax(as.integer(introns$V10), as.integer(introns$V11))
  cds <- cds[order(cds$start_norm, cds$end_norm), , drop = FALSE]
  introns <- introns[order(introns$start_norm, introns$end_norm), , drop = FALSE]

  gene_start <- min(as.integer(gene$V10), as.integer(gene$V11))
  gene_end <- max(as.integer(gene$V10), as.integer(gene$V11))
  strand <- ifelse(gene$V12 == "C", -1L, 1L)
  gene_symbol <- ifelse(nzchar(gene$V5), gene$V5, gene$V4)

  gene_row <- data.frame(
    species = species_row$species,
    display_name = species_row$display_name,
    scientific_name = species_row$scientific_name,
    source_database = species_row$source_database,
    assembly = species_row$assembly,
    gene_id = gene$V4,
    gene_symbol = gene_symbol,
    seqname = as.character(gene$V9),
    chr = species_row$chr,
    start = gene_start,
    end = gene_end,
    strand = strand,
    length_bp = gene_end - gene_start + 1L,
    stringsAsFactors = FALSE
  )

  transcript_id <- paste0(gene$V4, ".mRNA")
  transcript_row <- data.frame(
    species = species_row$species,
    display_name = species_row$display_name,
    scientific_name = species_row$scientific_name,
    source_database = species_row$source_database,
    assembly = species_row$assembly,
    gene_id = gene$V4,
    gene_symbol = gene_symbol,
    seqname = as.character(gene$V9),
    chr = species_row$chr,
    transcript_id = transcript_id,
    transcript_name = gene_symbol,
    biotype = "protein_coding",
    is_canonical = 1L,
    transcript_start = gene_start,
    transcript_end = gene_end,
    strand = strand,
    exon_count = nrow(cds),
    intron_count = nrow(introns),
    stringsAsFactors = FALSE
  )

  exon_rows <- data.frame(
    species = species_row$species,
    gene_id = gene$V4,
    gene_symbol = gene_symbol,
    transcript_id = transcript_id,
    transcript_name = gene_symbol,
    transcript_biotype = "protein_coding",
    is_canonical = 1L,
    exon_rank = seq_len(nrow(cds)),
    exon_id = paste0(gene$V4, ".cds", seq_len(nrow(cds))),
    seqname = as.character(gene$V9),
    chr = species_row$chr,
    start = cds$start_norm,
    end = cds$end_norm,
    strand = strand,
    source_database = species_row$source_database,
    stringsAsFactors = FALSE
  )

  intron_rows <- data.frame(
    species = species_row$species,
    gene_id = gene$V4,
    gene_symbol = gene_symbol,
    transcript_id = transcript_id,
    transcript_name = gene_symbol,
    intron_rank = seq_len(nrow(introns)),
    intron_id = paste0(gene$V4, ".intron", seq_len(nrow(introns))),
    seqname = as.character(gene$V9),
    chr = species_row$chr,
    start = introns$start_norm,
    end = introns$end_norm,
    strand = strand,
    source_database = species_row$source_database,
    stringsAsFactors = FALSE
  )

  list(gene = gene_row, transcript = transcript_row, exons = exon_rows, introns = intron_rows)
}

make_plot_exons <- function(exons, genes) {
  exons |>
    dplyr::left_join(
      genes |> dplyr::select("species", gene_start = "start", gene_end = "end"),
      by = "species"
    ) |>
    dplyr::mutate(
      xmin = .data$start,
      xmax = .data$end,
      y = 1L,
      ymin = transcript_y,
      strand = ifelse(.data$strand >= 0, "+", "-"),
      track = .data$species,
      individual = .data$species,
      gene = .data$gene_symbol,
      transcript = .data$transcript_name,
      transcripts = .data$transcript_name,
      feature = "CDS",
      type = "exon",
      exon_role = "CDS"
    ) |>
    dplyr::arrange(.data$species, .data$exon_rank)
}

fetch_window_sequence <- function(species_row, gene_row, sequence, padding) {
  start0 <- max(0L, gene_row$start - 1L - padding)
  end0 <- gene_row$end + padding
  dna <- substring(sequence, start0 + 1L, end0)
  fasta_name <- paste(species_row$species, species_row$chr, start0 + 1L, end0, sep = "|")
  fasta_path <- file.path(out_dir, "sequences", paste0(species_row$species, "_cox1_window.fa"))
  write_fasta(fasta_name, dna, fasta_path)
  data.frame(
    species = species_row$species,
    chr = species_row$chr,
    window_start = start0 + 1L,
    window_end = end0,
    window_start0 = start0,
    window_end0 = end0,
    fasta_name = fasta_name,
    fasta_file = file.path("sequences", basename(fasta_path)),
    length_bp = nchar(dna),
    stringsAsFactors = FALSE
  )
}

parse_paf <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    return(data.frame())
  }
  cols <- c(
    "qname", "qlen", "qstart", "qend", "strand", "tname", "tlen",
    "tstart", "tend", "nmatch", "alen", "mapq"
  )
  paf <- utils::read.table(
    path,
    sep = "\t",
    quote = "",
    comment.char = "",
    header = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE
  )
  if (nrow(paf) == 0L || ncol(paf) < length(cols)) {
    return(data.frame())
  }
  names(paf)[seq_along(cols)] <- cols
  paf <- paf[, seq_along(cols), drop = FALSE]
  numeric_cols <- c("qlen", "qstart", "qend", "tlen", "tstart", "tend", "nmatch", "alen", "mapq")
  paf[numeric_cols] <- lapply(paf[numeric_cols], as.integer)
  paf
}

classify_lastz_identity <- function(identity) {
  as.character(cut(
    identity,
    breaks = lastz_identity_breaks,
    labels = lastz_identity_labels,
    right = FALSE
  ))
}

run_lastz <- function(windows, min_len = lastz_min_len, min_identity = lastz_min_identity) {
  target <- windows[windows$species == "fission_yeast", , drop = FALSE]
  query <- windows[windows$species == "budding_yeast", , drop = FALSE]
  paf_path <- file.path(out_dir, "cox1_lastz.paf")

  if (!file.exists(lastz_bin)) {
    warning("LASTZ binary not found at ", lastz_bin, ". Skipping alignment.")
    return(list(paf = data.frame(), paf_path = paf_path, command = NA_character_))
  }

  target_fa <- file.path(out_dir, target$fasta_file)
  query_fa <- file.path(out_dir, query$fasta_file)
  args <- c(
    target_fa,
    query_fa,
    "--strand=both",
    "--chain",
    "--format=paf:minimap2",
    paste0("--output=", paf_path)
  )
  portable_command <- paste(
    c(
      "lastz",
      basename(target_fa),
      basename(query_fa),
      args[3:5],
      "--output=cox1_lastz.paf"
    ),
    collapse = " "
  )
  message("Running LASTZ: ", paste(c(lastz_bin, args), collapse = " "))
  status <- system2(lastz_bin, args = args, stdout = TRUE, stderr = TRUE)
  exit_status <- attr(status, "status") %||% 0L
  if (!identical(as.integer(exit_status), 0L)) {
    warning("LASTZ exited with status ", exit_status, ": ", paste(status, collapse = "\n"))
  }

  paf <- parse_paf(paf_path)
  if (nrow(paf) == 0L) {
    return(list(paf = paf, paf_path = paf_path, command = portable_command))
  }

  paf <- paf |>
    dplyr::mutate(identity = 100 * .data$nmatch / pmax(.data$alen, 1L)) |>
    dplyr::filter(.data$alen >= min_len, .data$identity >= min_identity) |>
    dplyr::arrange(.data$tstart, .data$qstart)

  list(paf = paf, paf_path = paf_path, command = portable_command)
}

make_nuclinks <- function(paf, windows) {
  if (nrow(paf) == 0L) {
    return(data.frame())
  }
  fission <- windows[windows$species == "fission_yeast", , drop = FALSE]
  budding <- windows[windows$species == "budding_yeast", , drop = FALSE]

  paf |>
    dplyr::mutate(
      track = "link_fission_budding",
      tspecies = "fission_yeast",
      tchr = fission$chr,
      tstart = fission$window_start0 + .data$tstart + 1L,
      tend = fission$window_start0 + .data$tend,
      qspecies = "budding_yeast",
      qchr = budding$chr,
      qstart = budding$window_start0 + .data$qstart + 1L,
      qend = budding$window_start0 + .data$qend,
      group = paste0("lastz_", dplyr::row_number()),
      score = .data$nmatch,
      alignment_length = .data$alen,
      identity = round(.data$identity, 3),
      identity_bin = classify_lastz_identity(.data$identity)
    ) |>
    dplyr::select(
      "track", "tspecies", "tchr", "tstart", "tend",
      "qspecies", "qchr", "qstart", "qend", "strand",
      "group", "score", "alignment_length", "identity", "identity_bin"
    )
}

overlap_bp <- function(a_start, a_end, b_start, b_end) {
  pmax(0L, pmin(a_end, b_end) - pmax(a_start, b_start) + 1L)
}

make_exon_homology_candidates <- function(links, exons) {
  if (nrow(links) == 0L) {
    return(data.frame())
  }
  fission_exons <- exons[exons$species == "fission_yeast", , drop = FALSE]
  budding_exons <- exons[exons$species == "budding_yeast", , drop = FALSE]

  rows <- list()
  for (i in seq_len(nrow(links))) {
    link <- links[i, , drop = FALSE]
    f_ov <- overlap_bp(fission_exons$start, fission_exons$end, link$tstart, link$tend)
    b_ov <- overlap_bp(budding_exons$start, budding_exons$end, link$qstart, link$qend)
    f_hits <- fission_exons[f_ov > 0L, , drop = FALSE]
    b_hits <- budding_exons[b_ov > 0L, , drop = FALSE]
    if (nrow(f_hits) == 0L || nrow(b_hits) == 0L) {
      next
    }
    f_hits$fission_overlap_bp <- f_ov[f_ov > 0L]
    b_hits$budding_overlap_bp <- b_ov[b_ov > 0L]
    for (f in seq_len(nrow(f_hits))) {
      for (b in seq_len(nrow(b_hits))) {
        rows[[length(rows) + 1L]] <- data.frame(
          link_group = link$group,
          strand = link$strand,
          fission_exon_id = f_hits$exon_id[[f]],
          fission_exon_rank = f_hits$exon_rank[[f]],
          fission_start = f_hits$start[[f]],
          fission_end = f_hits$end[[f]],
          fission_overlap_bp = f_hits$fission_overlap_bp[[f]],
          budding_exon_id = b_hits$exon_id[[b]],
          budding_exon_rank = b_hits$exon_rank[[b]],
          budding_start = b_hits$start[[b]],
          budding_end = b_hits$end[[b]],
          budding_overlap_bp = b_hits$budding_overlap_bp[[b]],
          link_identity = link$identity,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(data.frame())
  }

  dplyr::bind_rows(rows) |>
    dplyr::mutate(
      min_overlap_bp = pmin(.data$fission_overlap_bp, .data$budding_overlap_bp)
    ) |>
    dplyr::arrange(.data$fission_start, .data$budding_start, dplyr::desc(.data$min_overlap_bp))
}

rank_exon_homology_candidates <- function(candidates) {
  if (nrow(candidates) == 0L) {
    return(data.frame())
  }

  candidates |>
    dplyr::group_by(
      .data$fission_exon_id, .data$fission_exon_rank, .data$fission_start, .data$fission_end,
      .data$budding_exon_id, .data$budding_exon_rank, .data$budding_start, .data$budding_end
    ) |>
    dplyr::summarise(
      link_count = dplyr::n_distinct(.data$link_group),
      max_fission_overlap_bp = max(.data$fission_overlap_bp),
      max_budding_overlap_bp = max(.data$budding_overlap_bp),
      max_min_overlap_bp = max(.data$min_overlap_bp),
      max_link_identity = max(.data$link_identity),
      strands = paste(sort(unique(.data$strand)), collapse = ","),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$fission_exon_id) |>
    dplyr::arrange(dplyr::desc(.data$max_min_overlap_bp), dplyr::desc(.data$max_link_identity), .by_group = TRUE) |>
    dplyr::mutate(fission_pair_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$budding_exon_id) |>
    dplyr::arrange(dplyr::desc(.data$max_min_overlap_bp), dplyr::desc(.data$max_link_identity), .by_group = TRUE) |>
    dplyr::mutate(budding_pair_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(reciprocal_best = .data$fission_pair_rank == 1L & .data$budding_pair_rank == 1L) |>
    dplyr::arrange(.data$fission_start, .data$budding_start)
}

write_selected_gff3 <- function(exons, species_id, path) {
  df <- exons[exons$species == species_id, , drop = FALSE]
  con <- file(path, open = "wt")
  on.exit(close(con), add = TRUE)
  writeLines("##gff-version 3", con)

  tx_start <- min(df$start)
  tx_end <- max(df$end)
  strand <- unique(df$strand)[[1L]]
  gene_id <- unique(df$gene_id)[[1L]]
  gene_name <- unique(df$gene_symbol)[[1L]]
  tx_id <- unique(df$transcript_id)[[1L]]
  tx_name <- unique(df$transcript_name)[[1L]]
  source <- unique(df$source_database)[[1L]]
  attrs_tx <- paste0(
    "ID=", tx_id,
    ";Parent=", gene_id,
    ";gene_id=", gene_id,
    ";gene_name=", gene_name,
    ";transcript_id=", tx_id,
    ";transcript_name=", tx_name
  )
  writeLines(paste(df$chr[[1L]], source, "mRNA", tx_start, tx_end, ".", strand, ".", attrs_tx, sep = "\t"), con)
  for (i in seq_len(nrow(df))) {
    attrs_exon <- paste0(
      "ID=", df$exon_id[[i]],
      ";Parent=", df$transcript_id[[i]],
      ";gene_id=", df$gene_id[[i]],
      ";gene_name=", df$gene_symbol[[i]],
      ";transcript_id=", df$transcript_id[[i]],
      ";transcript_name=", df$transcript_name[[i]],
      ";rank=", df$exon_rank[[i]]
    )
    writeLines(paste(df$chr[[i]], source, "CDS", df$start[[i]], df$end[[i]], ".", df$strand[[i]], ".", attrs_exon, sep = "\t"), con)
  }
  invisible(path)
}

message("Downloading source annotation and sequence files")
pombase_gff3_path <- download_file_once(pombase_gff3_url, file.path(raw_dir, "pombase_all_chromosomes.gff3.gz"))
pombase_fasta_gz_path <- download_file_once(pombase_fasta_url, file.path(raw_dir, "pombase_all_chromosomes.fa.gz"))
pombase_ortholog_path <- download_file_once(pombase_ortholog_url, file.path(raw_dir, "pombe_cerevisiae_orthologs.tsv"))
sgd_features_path <- download_file_once(sgd_features_url, file.path(raw_dir, "SGD_features.tab"))
sgd_chrmt_path <- download_file_once(sgd_chrmt_fasta_url, file.path(raw_dir, "sgd_chrmt.fsa"))

message("Parsing PomBase and SGD annotations")
pombase_gff <- read_pombase_gff3(pombase_gff3_path)
sgd_features <- read_sgd_features(sgd_features_path)
orthologs <- readr::read_tsv(
  pombase_ortholog_path,
  col_names = c("pombe_gene_id", "sgd_gene_id"),
  show_col_types = FALSE
)
ortholog_row <- orthologs |>
  dplyr::filter(.data$pombe_gene_id == "SPMIT.01", .data$sgd_gene_id == "Q0045")
if (nrow(ortholog_row) != 1L) {
  stop("Expected PomBase ortholog mapping SPMIT.01 -> Q0045", call. = FALSE)
}

parsed <- list(
  fission_yeast = build_pombase_gene(pombase_gff, species[species$species == "fission_yeast", , drop = FALSE]),
  budding_yeast = build_sgd_gene(sgd_features, species[species$species == "budding_yeast", , drop = FALSE])
)

genes <- dplyr::bind_rows(lapply(parsed, `[[`, "gene"))
transcripts <- dplyr::bind_rows(lapply(parsed, `[[`, "transcript"))
exons <- dplyr::bind_rows(lapply(parsed, `[[`, "exons"))
introns <- dplyr::bind_rows(lapply(parsed, `[[`, "introns"))
plot_exons <- make_plot_exons(exons, genes)

message("Reading genome sequences and writing LASTZ windows")
pombase_fasta_path <- file.path(raw_dir, "pombase_all_chromosomes.fa")
if (!file.exists(pombase_fasta_path) || file.info(pombase_fasta_path)$size == 0L) {
  con_in <- gzfile(pombase_fasta_gz_path, open = "rt")
  con_out <- file(pombase_fasta_path, open = "wt")
  on.exit(close(con_in), add = TRUE)
  on.exit(close(con_out), add = TRUE)
  writeLines(readLines(con_in, warn = FALSE), con_out)
  close(con_in)
  close(con_out)
}
pombase_sequences <- read_fasta(pombase_fasta_path)
sgd_sequences <- read_fasta(sgd_chrmt_path)

windows <- dplyr::bind_rows(lapply(seq_len(nrow(species)), function(i) {
  row <- species[i, , drop = FALSE]
  gene_row <- genes[genes$species == row$species, , drop = FALSE]
  sequence <- if (row$species == "fission_yeast") {
    pombase_sequences[[row$fasta_seqname]]
  } else {
    sgd_sequences[[1L]]
  }
  fetch_window_sequence(row, gene_row, sequence, window_padding)
}))

alignment <- run_lastz(windows)
nuclinks <- make_nuclinks(alignment$paf, windows)
exon_homology <- make_exon_homology_candidates(nuclinks, exons)
exon_homology_ranked <- rank_exon_homology_candidates(exon_homology)

genes_meta <- genes |>
  dplyr::select(
    "species",
    gene_start = "start",
    gene_end = "end",
    gene_strand = "strand",
    gene_length_bp = "length_bp"
  )
windows_meta <- windows |>
  dplyr::rename(window_length_bp = "length_bp")
species_out <- species |>
  dplyr::left_join(genes_meta, by = "species") |>
  dplyr::left_join(windows_meta, by = c("species", "chr"))

readr::write_tsv(species_out, file.path(out_dir, "cox1_species.tsv"))
readr::write_tsv(genes, file.path(out_dir, "cox1_genes.tsv"))
readr::write_tsv(transcripts, file.path(out_dir, "cox1_transcripts.tsv"))
readr::write_tsv(exons, file.path(out_dir, "cox1_cds_exons.tsv"))
readr::write_tsv(introns, file.path(out_dir, "cox1_introns.tsv"))
readr::write_tsv(plot_exons, file.path(out_dir, "cox1_plot_exons.tsv"))
readr::write_tsv(nuclinks, file.path(out_dir, "cox1_nuclinks_lastz.tsv"))
readr::write_tsv(exon_homology, file.path(out_dir, "cox1_exon_homology_candidates.tsv"))
readr::write_tsv(exon_homology_ranked, file.path(out_dir, "cox1_exon_homology_ranked.tsv"))

write_selected_gff3(plot_exons, "fission_yeast", file.path(out_dir, "annotations", "fission_yeast.gff3"))
write_selected_gff3(plot_exons, "budding_yeast", file.path(out_dir, "annotations", "budding_yeast.gff3"))

source_keys <- c(
  "pombase_gff3",
  "pombase_sequence",
  "pombase_orthologs",
  "sgd_features",
  "sgd_mitochondrial_sequence"
)
source_urls <- c(
  pombase_gff3_url,
  pombase_fasta_url,
  pombase_ortholog_url,
  sgd_features_url,
  sgd_chrmt_fasta_url
)
source_paths <- c(
  pombase_gff3_path,
  pombase_fasta_gz_path,
  pombase_ortholog_path,
  sgd_features_path,
  sgd_chrmt_path
)
source_checksums <- unname(tools::md5sum(source_paths))
source_retrieved_on <- format(file.info(source_paths)$mtime, "%Y-%m-%d", tz = "UTC")

provenance <- data.frame(
  key = c(
    source_keys,
    paste0(source_keys, "_md5"),
    paste0(source_keys, "_retrieved_on"),
    "ortholog_mapping",
    "window_padding_bp",
    "lastz_min_alignment_length",
    "lastz_min_identity",
    "lastz_binary",
    "lastz_command",
    "rscript"
  ),
  value = c(
    source_urls,
    source_checksums,
    source_retrieved_on,
    "SPMIT.01 -> Q0045",
    as.character(window_padding),
    as.character(lastz_min_len),
    as.character(lastz_min_identity),
    basename(lastz_bin),
    alignment$command %||% NA_character_,
    paste("R", getRversion())
  ),
  stringsAsFactors = FALSE
)
readr::write_tsv(provenance, file.path(out_dir, "cox1_provenance.tsv"))

readme <- c(
  "# cox1/COX1 Yeast Pairwise Demo Candidate",
  "",
  "Generated from PomBase and SGD downloads whose URLs, retrieval dates,",
  "and exact MD5 checksums are recorded in `cox1_provenance.tsv`.",
  "",
  "This dataset supports a pairwise ggexon tutorial with two mitochondrial",
  "annotation tracks and one middle `geom_nuclink()` panel. Unlike the CD44",
  "human/mouse tutorial, this is not an alternative-transcript example. It",
  "shows conserved mitochondrial cytochrome c oxidase subunit 1 coding sequence",
  "across different organellar intron architectures in fission and budding",
  "yeast.",
  "",
  "Key files:",
  "",
  "- `cox1_species.tsv`: species, source, gene, and plotted window metadata.",
  "- `cox1_genes.tsv`: gene-level coordinates.",
  "- `cox1_transcripts.tsv`: one protein-coding transcript per species.",
  "- `cox1_cds_exons.tsv`: CDS intervals from PomBase and SGD.",
  "- `cox1_introns.tsv`: annotated intron intervals from PomBase and SGD.",
  "- `cox1_plot_exons.tsv`: plot-ready CDS intervals for `geom_exon()`.",
  paste0(
    "- `cox1_nuclinks_lastz.tsv`: LASTZ-derived genomic interval links retained at ",
    "alignment length >= ", lastz_min_len, " bp and identity >= ", lastz_min_identity, "%."
  ),
  "- `cox1_exon_homology_candidates.tsv`: exon-homology candidates from overlaps",
  "  between LASTZ blocks and CDS intervals.",
  "- `cox1_exon_homology_ranked.tsv`: one row per exon-pair candidate with",
  "  reciprocal-best ranks.",
  "- `annotations/*.gff3`: compact selected-transcript GFF3 files.",
  "- `sequences/*.fa`: genomic DNA windows used for LASTZ.",
  "- `cox1_provenance.tsv`: source URLs, checksums, dates, and portable tool provenance."
)
writeLines(readme, file.path(out_dir, "README.md"))

message("Wrote cox1 yeast demo candidate files to ", out_dir)
message("CDS blocks:")
print(transcripts[, c("species", "gene_symbol", "transcript_id", "exon_count", "intron_count")], row.names = FALSE)
message("LASTZ links retained: ", nrow(nuclinks))
message("Exon homology candidate rows: ", nrow(exon_homology))
