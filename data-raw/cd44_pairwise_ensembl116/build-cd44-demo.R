# Generate the bundled CD44/Cd44 pairwise-alignment demo candidate.
#
# The script downloads current Ensembl REST gene models for human CD44 and
# mouse Cd44, downloads matching GRCh38/GRCm39 genomic windows from UCSC,
# selects a small representative isoform set, runs LASTZ on the genomic DNA
# windows, and writes plot-ready tables under inst/extdata.

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("data-raw/cd44_pairwise_ensembl116/build-cd44-demo.R", mustWork = FALSE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
if (!dir.exists(file.path(repo_root, "inst"))) {
  repo_root <- normalizePath(getwd(), mustWork = TRUE)
}

required_packages <- c("jsonlite", "dplyr", "readr", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop("Missing required R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

out_dir <- file.path(repo_root, "inst", "extdata", "cd44_pairwise_ensembl116")
raw_dir <- file.path(repo_root, "data-raw", "cd44_pairwise_ensembl116", "downloads")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "annotations"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "sequences"), showWarnings = FALSE, recursive = TRUE)

lastz_bin <- Sys.getenv("LASTZ", unset = "/opt/homebrew/bin/lastz")
lastz_bin <- normalizePath(lastz_bin, mustWork = FALSE)
rscript_bin <- normalizePath(file.path(R.home("bin"), "Rscript"), mustWork = FALSE)

ensembl_base <- "https://rest.ensembl.org"
ucsc_base <- "https://api.genome.ucsc.edu"
window_padding <- 5000L
isoform_y_step <- 0.72
isoform_y_start <- 0.2

species <- data.frame(
  species = c("human", "mouse"),
  display_name = c("Human", "Mouse"),
  scientific_name = c("Homo sapiens", "Mus musculus"),
  ensembl_species = c("homo_sapiens", "mus_musculus"),
  ensembl_gene_id = c("ENSG00000026508", "ENSMUSG00000005087"),
  gene_symbol = c("CD44", "Cd44"),
  assembly = c("GRCh38", "GRCm39"),
  ucsc_genome = c("hg38", "mm39"),
  ucsc_chrom = c("chr11", "chr2"),
  stringsAsFactors = FALSE
)
species$species_order <- seq_len(nrow(species))

selection_anchors <- data.frame(
  species = c(rep("human", 4), rep("mouse", 4)),
  transcript_name = c(
    "CD44-208", "CD44-201", "CD44-210", "CD44-242",
    "Cd44-201", "Cd44-203", "Cd44-204", "Cd44-208"
  ),
  selection_order = rep(seq_len(4L), 2),
  selection_reason = c(
    "RefSeq-backed Ensembl canonical transcript (NM_000610)",
    "RefSeq-backed shorter CD44 splice isoform (NM_001001391)",
    "RefSeq-backed intermediate CD44 splice isoform (NM_001202555)",
    "RefSeq-backed expanded CD44 splice isoform (NM_001440324/NM_001440326)",
    "RefSeq-backed Ensembl canonical transcript (NM_009851)",
    "RefSeq-backed shorter Cd44 splice isoform (NM_001039151)",
    "RefSeq-backed intermediate Cd44 splice isoform (NM_001177787)",
    "RefSeq-backed expanded Cd44 splice isoform (NM_001177785)"
  ),
  stringsAsFactors = FALSE
)

download_file_once <- function(url, dest) {
  if (!file.exists(dest) || file.info(dest)$size == 0L) {
    message("Downloading ", url)
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

read_json_url <- function(url, dest) {
  download_file_once(url, dest)
  jsonlite::read_json(dest, simplifyVector = FALSE)
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

reverse_complement <- function(x) {
  chars <- strsplit(toupper(x), "", fixed = TRUE)[[1L]]
  comp <- c(A = "T", C = "G", G = "C", T = "A", N = "N")
  paste(rev(unname(comp[chars] %||% "N")), collapse = "")
}

parse_ensembl_gene <- function(gene, species_row) {
  transcripts <- gene$Transcript %||% list()
  tx_rows <- list()
  exon_rows <- list()

  for (tx in transcripts) {
    exons <- tx$Exon %||% list()
    exon_ids <- vapply(exons, function(exon) exon$id %||% NA_character_, character(1))
    exon_ids <- exon_ids[!is.na(exon_ids)]
    exon_signature <- paste(exon_ids, collapse = "|")

    tx_rows[[length(tx_rows) + 1L]] <- data.frame(
      species = species_row$species,
      display_name = species_row$display_name,
      scientific_name = species_row$scientific_name,
      assembly = species_row$assembly,
      ensembl_species = species_row$ensembl_species,
      gene_id = gene$id,
      gene_symbol = gene$display_name %||% species_row$gene_symbol,
      seqname = as.character(gene$seq_region_name),
      transcript_id = tx$id,
      transcript_name = tx$display_name %||% tx$id,
      biotype = tx$biotype %||% NA_character_,
      is_canonical = as.integer(tx$is_canonical %||% 0L),
      transcript_start = as.integer(tx$start),
      transcript_end = as.integer(tx$end),
      strand = as.integer(tx$strand),
      exon_count = length(exons),
      exon_signature = exon_signature,
      stringsAsFactors = FALSE
    )

    if (length(exons) > 0L) {
      for (rank in seq_along(exons)) {
        exon <- exons[[rank]]
        exon_rows[[length(exon_rows) + 1L]] <- data.frame(
          species = species_row$species,
          gene_id = gene$id,
          gene_symbol = gene$display_name %||% species_row$gene_symbol,
          transcript_id = tx$id,
          transcript_name = tx$display_name %||% tx$id,
          transcript_biotype = tx$biotype %||% NA_character_,
          is_canonical = as.integer(tx$is_canonical %||% 0L),
          exon_rank = rank,
          exon_id = exon$id %||% paste0(tx$id, "_exon_", rank),
          seqname = as.character(exon$seq_region_name %||% gene$seq_region_name),
          start = as.integer(exon$start),
          end = as.integer(exon$end),
          strand = as.integer(exon$strand %||% tx$strand %||% gene$strand),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  list(
    gene = data.frame(
      species = species_row$species,
      display_name = species_row$display_name,
      scientific_name = species_row$scientific_name,
      assembly = species_row$assembly,
      ensembl_species = species_row$ensembl_species,
      gene_id = gene$id,
      gene_symbol = gene$display_name %||% species_row$gene_symbol,
      seqname = as.character(gene$seq_region_name),
      start = as.integer(gene$start),
      end = as.integer(gene$end),
      strand = as.integer(gene$strand),
      length_bp = as.integer(gene$end) - as.integer(gene$start) + 1L,
      stringsAsFactors = FALSE
    ),
    transcripts = dplyr::bind_rows(tx_rows),
    exons = dplyr::bind_rows(exon_rows)
  )
}

transcript_exon_sets <- function(exons) {
  split(exons$exon_id, exons$transcript_id)
}

jaccard_distance <- function(a, b) {
  union_n <- length(union(a, b))
  if (union_n == 0L) {
    return(0)
  }
  1 - length(intersect(a, b)) / union_n
}

select_isoforms_one_species <- function(transcripts,
                                        exons,
                                        anchors = NULL,
                                        n = 4L,
                                        min_span_fraction = 0.75) {
  gene_start <- min(transcripts$transcript_start, na.rm = TRUE)
  gene_end <- max(transcripts$transcript_end, na.rm = TRUE)
  gene_span <- gene_end - gene_start + 1L

  tx <- transcripts |>
    dplyr::mutate(
      span_fraction = (transcript_end - transcript_start + 1L) / gene_span,
      eligible = biotype == "protein_coding" & span_fraction >= min_span_fraction
    )

  candidates <- tx |>
    dplyr::filter(.data$eligible) |>
    dplyr::arrange(dplyr::desc(.data$is_canonical), .data$exon_count, .data$transcript_name)
  if (nrow(candidates) == 0L) {
    candidates <- tx |>
      dplyr::filter(.data$biotype == "protein_coding") |>
      dplyr::arrange(dplyr::desc(.data$is_canonical), .data$exon_count, .data$transcript_name)
  }

  selected <- character()
  reasons <- character()

  if (!is.null(anchors) && nrow(anchors) > 0L) {
    anchor_rows <- anchors |>
      dplyr::arrange(.data$selection_order) |>
      dplyr::inner_join(
        tx |> dplyr::select("transcript_id", "transcript_name"),
        by = "transcript_name"
      )
    selected <- anchor_rows$transcript_id
    reasons <- anchor_rows$selection_reason
  }

  exon_sets <- transcript_exon_sets(exons)

  canonical <- candidates$transcript_id[candidates$is_canonical == 1L]
  if (length(canonical) > 0L && !canonical[[1L]] %in% selected) {
    selected <- c(selected, canonical[[1L]])
    reasons <- c(reasons, "canonical Ensembl transcript")
  }

  min_exon <- candidates |>
    dplyr::filter(!.data$transcript_id %in% selected) |>
    dplyr::arrange(.data$exon_count, .data$transcript_start, .data$transcript_name)
  if (nrow(min_exon) > 0L && length(selected) < n) {
    selected <- c(selected, min_exon$transcript_id[[1L]])
    reasons <- c(reasons, "fewest-exon full-locus protein-coding isoform")
  }

  max_exon <- candidates |>
    dplyr::filter(!.data$transcript_id %in% selected) |>
    dplyr::arrange(dplyr::desc(.data$exon_count), .data$transcript_name)
  if (nrow(max_exon) > 0L && length(selected) < n) {
    selected <- c(selected, max_exon$transcript_id[[1L]])
    reasons <- c(reasons, "most-exon full-locus protein-coding isoform")
  }

  while (length(selected) < n) {
    remaining <- candidates |>
      dplyr::filter(!.data$transcript_id %in% selected)
    if (nrow(remaining) == 0L) {
      break
    }

    scores <- vapply(remaining$transcript_id, function(id) {
      min(vapply(selected, function(sel) {
        jaccard_distance(exon_sets[[id]] %||% character(), exon_sets[[sel]] %||% character())
      }, numeric(1)))
    }, numeric(1))
    pick <- remaining$transcript_id[[which.max(scores)]]
    selected <- c(selected, pick)
    reasons <- c(reasons, sprintf("maximizes exon-set distance from already selected isoforms (min Jaccard distance %.3f)", max(scores)))
  }

  selected <- selected[seq_len(min(length(selected), n))]
  reasons <- reasons[seq_along(selected)]

  tx |>
    dplyr::filter(.data$transcript_id %in% selected) |>
    dplyr::mutate(
      selection_order = match(.data$transcript_id, selected),
      selection_reason = reasons[match(.data$transcript_id, selected)]
    ) |>
    dplyr::arrange(.data$selection_order)
}

make_unique_exons <- function(exons) {
  exons |>
    dplyr::distinct(.data$species, .data$gene_id, .data$gene_symbol, .data$exon_id,
                    .data$seqname, .data$start, .data$end, .data$strand) |>
    dplyr::group_by(.data$species) |>
    dplyr::arrange(.data$start, .data$end, .by_group = TRUE) |>
    dplyr::mutate(genomic_exon_index = dplyr::row_number()) |>
    dplyr::ungroup()
}

make_plot_exons <- function(selected, exons, genes, species_info) {
  selected_exons <- exons |>
    dplyr::inner_join(
      selected |>
        dplyr::select("species", "transcript_id", "transcript_name",
                      "selection_order", "selection_reason"),
      by = c("species", "transcript_id", "transcript_name")
    ) |>
    dplyr::left_join(
      genes |> dplyr::select("species", gene_start = "start", gene_end = "end"),
      by = "species"
    ) |>
    dplyr::left_join(
      species_info |> dplyr::select("species", "ucsc_chrom"),
      by = "species"
    ) |>
    dplyr::mutate(
      xmin = .data$start,
      xmax = .data$end,
      y = .data$selection_order,
      ymin = isoform_y_start + (.data$selection_order - 1L) * isoform_y_step,
      strand = ifelse(.data$strand >= 0, "+", "-"),
      chr = .data$ucsc_chrom,
      track = .data$species,
      individual = .data$species,
      gene = .data$gene_symbol,
      transcript = .data$transcript_name,
      transcripts = .data$transcript_name,
      feature = "exon",
      type = "exon"
    ) |>
    dplyr::arrange(.data$species, .data$selection_order, .data$exon_rank)

  selected_exons
}

make_common_exons <- function(selected, exons) {
  selected_ids <- selected |>
    dplyr::select("species", "transcript_id")

  total_by_species <- selected_ids |>
    dplyr::count(.data$species, name = "selected_isoform_count")

  exons |>
    dplyr::inner_join(selected_ids, by = c("species", "transcript_id")) |>
    dplyr::distinct(.data$species, .data$transcript_id, .data$exon_id, .keep_all = TRUE) |>
    dplyr::count(.data$species, .data$exon_id, .data$seqname, .data$start, .data$end, .data$strand,
                 name = "isoforms_with_exon") |>
    dplyr::left_join(total_by_species, by = "species") |>
    dplyr::mutate(
      present_in_all_selected_isoforms = .data$isoforms_with_exon == .data$selected_isoform_count
    ) |>
    dplyr::arrange(.data$species, .data$start, .data$end)
}

fetch_ucsc_sequence <- function(species_row, gene_row, padding) {
  start0 <- max(0L, gene_row$start - 1L - padding)
  end0 <- gene_row$end + padding
  url <- paste0(
    ucsc_base, "/getData/sequence?genome=", species_row$ucsc_genome,
    ";chrom=", species_row$ucsc_chrom,
    ";start=", start0,
    ";end=", end0
  )
  dest <- file.path(raw_dir, paste0(species_row$species, "_", species_row$gene_symbol, "_sequence.json"))
  seq_json <- read_json_url(url, dest)
  dna <- toupper(seq_json$dna)
  if (!is.character(dna) || !nzchar(dna)) {
    stop("No DNA returned for ", species_row$species, call. = FALSE)
  }
  fasta_name <- paste(species_row$species, species_row$ucsc_chrom, start0 + 1L, end0, sep = "|")
  fasta_path <- file.path(out_dir, "sequences", paste0(species_row$species, "_cd44_window.fa"))
  write_fasta(fasta_name, dna, fasta_path)
  data.frame(
    species = species_row$species,
    ucsc_genome = species_row$ucsc_genome,
    chr = species_row$ucsc_chrom,
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
  names(paf)[seq_along(cols)] <- cols
  paf <- paf[, seq_along(cols), drop = FALSE]
  numeric_cols <- c("qlen", "qstart", "qend", "tlen", "tstart", "tend", "nmatch", "alen", "mapq")
  paf[numeric_cols] <- lapply(paf[numeric_cols], as.integer)
  paf
}

run_lastz <- function(windows, min_len = 80L, min_identity = 55) {
  target <- windows[windows$species == "human", , drop = FALSE]
  query <- windows[windows$species == "mouse", , drop = FALSE]
  paf_path <- file.path(out_dir, "cd44_lastz.paf")

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
  message("Running LASTZ: ", paste(c(lastz_bin, args), collapse = " "))
  status <- system2(lastz_bin, args = args, stdout = TRUE, stderr = TRUE)
  exit_status <- attr(status, "status") %||% 0L
  if (!identical(as.integer(exit_status), 0L)) {
    warning("LASTZ exited with status ", exit_status, ": ", paste(status, collapse = "\n"))
  }

  paf <- parse_paf(paf_path)
  if (nrow(paf) == 0L) {
    return(list(paf = paf, paf_path = paf_path, command = paste(c(lastz_bin, args), collapse = " ")))
  }

  paf <- paf |>
    dplyr::mutate(identity = 100 * .data$nmatch / pmax(.data$alen, 1L)) |>
    dplyr::filter(.data$alen >= min_len, .data$identity >= min_identity) |>
    dplyr::arrange(.data$tstart, .data$qstart)

  list(paf = paf, paf_path = paf_path, command = paste(c(lastz_bin, args), collapse = " "))
}

make_nuclinks <- function(paf, windows) {
  if (nrow(paf) == 0L) {
    return(data.frame())
  }
  human <- windows[windows$species == "human", , drop = FALSE]
  mouse <- windows[windows$species == "mouse", , drop = FALSE]

  paf |>
    dplyr::mutate(
      track = "link_human_mouse",
      tspecies = "human",
      tchr = human$chr,
      tstart = human$window_start0 + .data$tstart + 1L,
      tend = human$window_start0 + .data$tend,
      qspecies = "mouse",
      qchr = mouse$chr,
      qstart = mouse$window_start0 + .data$qstart + 1L,
      qend = mouse$window_start0 + .data$qend,
      group = paste0("lastz_", dplyr::row_number()),
      score = .data$nmatch,
      alignment_length = .data$alen,
      identity = round(.data$identity, 3)
    ) |>
    dplyr::select(
      "track", "tspecies", "tchr", "tstart", "tend",
      "qspecies", "qchr", "qstart", "qend", "strand",
      "group", "score", "alignment_length", "identity"
    )
}

overlap_bp <- function(a_start, a_end, b_start, b_end) {
  pmax(0L, pmin(a_end, b_end) - pmax(a_start, b_start) + 1L)
}

make_exon_homology_candidates <- function(links, unique_exons) {
  if (nrow(links) == 0L) {
    return(data.frame())
  }
  human_exons <- unique_exons[unique_exons$species == "human", , drop = FALSE]
  mouse_exons <- unique_exons[unique_exons$species == "mouse", , drop = FALSE]

  rows <- list()
  for (i in seq_len(nrow(links))) {
    link <- links[i, , drop = FALSE]
    h_ov <- overlap_bp(human_exons$start, human_exons$end, link$tstart, link$tend)
    m_ov <- overlap_bp(mouse_exons$start, mouse_exons$end, link$qstart, link$qend)
    h_hits <- human_exons[h_ov > 0L, , drop = FALSE]
    m_hits <- mouse_exons[m_ov > 0L, , drop = FALSE]
    if (nrow(h_hits) == 0L || nrow(m_hits) == 0L) {
      next
    }
    h_hits$human_overlap_bp <- h_ov[h_ov > 0L]
    m_hits$mouse_overlap_bp <- m_ov[m_ov > 0L]
    for (h in seq_len(nrow(h_hits))) {
      for (m in seq_len(nrow(m_hits))) {
        rows[[length(rows) + 1L]] <- data.frame(
          link_group = link$group,
          strand = link$strand,
          human_exon_id = h_hits$exon_id[[h]],
          human_exon_index = h_hits$genomic_exon_index[[h]],
          human_start = h_hits$start[[h]],
          human_end = h_hits$end[[h]],
          human_overlap_bp = h_hits$human_overlap_bp[[h]],
          mouse_exon_id = m_hits$exon_id[[m]],
          mouse_exon_index = m_hits$genomic_exon_index[[m]],
          mouse_start = m_hits$start[[m]],
          mouse_end = m_hits$end[[m]],
          mouse_overlap_bp = m_hits$mouse_overlap_bp[[m]],
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
      min_overlap_bp = pmin(.data$human_overlap_bp, .data$mouse_overlap_bp)
    ) |>
    dplyr::arrange(.data$human_start, .data$mouse_start, dplyr::desc(.data$min_overlap_bp))
}

rank_exon_homology_candidates <- function(candidates) {
  if (nrow(candidates) == 0L) {
    return(data.frame())
  }

  candidates |>
    dplyr::group_by(
      .data$human_exon_id, .data$human_exon_index, .data$human_start, .data$human_end,
      .data$mouse_exon_id, .data$mouse_exon_index, .data$mouse_start, .data$mouse_end
    ) |>
    dplyr::summarise(
      link_count = dplyr::n_distinct(.data$link_group),
      max_human_overlap_bp = max(.data$human_overlap_bp),
      max_mouse_overlap_bp = max(.data$mouse_overlap_bp),
      max_min_overlap_bp = max(.data$min_overlap_bp),
      max_link_identity = max(.data$link_identity),
      strands = paste(sort(unique(.data$strand)), collapse = ","),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$human_exon_id) |>
    dplyr::arrange(dplyr::desc(.data$max_min_overlap_bp), dplyr::desc(.data$max_link_identity), .by_group = TRUE) |>
    dplyr::mutate(human_pair_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$mouse_exon_id) |>
    dplyr::arrange(dplyr::desc(.data$max_min_overlap_bp), dplyr::desc(.data$max_link_identity), .by_group = TRUE) |>
    dplyr::mutate(mouse_pair_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(reciprocal_best = .data$human_pair_rank == 1L & .data$mouse_pair_rank == 1L) |>
    dplyr::arrange(.data$human_start, .data$mouse_start)
}

write_selected_gff3 <- function(selected_exons, species_id, path) {
  df <- selected_exons[selected_exons$species == species_id, , drop = FALSE]
  con <- file(path, open = "wt")
  on.exit(close(con), add = TRUE)
  writeLines("##gff-version 3", con)
  for (tx_id in unique(df$transcript_id)) {
    tx <- df[df$transcript_id == tx_id, , drop = FALSE]
    tx_start <- min(tx$start)
    tx_end <- max(tx$end)
    strand <- unique(tx$strand)[[1L]]
    gene_id <- unique(tx$gene_id)[[1L]]
    gene_name <- unique(tx$gene_symbol)[[1L]]
    tx_name <- unique(tx$transcript_name)[[1L]]
    attrs_tx <- paste0(
      "ID=", tx_id,
      ";Parent=", gene_id,
      ";gene_id=", gene_id,
      ";gene_name=", gene_name,
      ";transcript_id=", tx_id,
      ";transcript_name=", tx_name
    )
    writeLines(paste(tx$chr[[1L]], "EnsemblREST", "mRNA", tx_start, tx_end, ".", strand, ".", attrs_tx, sep = "\t"), con)
    for (i in seq_len(nrow(tx))) {
      attrs_exon <- paste0(
        "ID=", tx$transcript_id[[i]], ".exon", tx$exon_rank[[i]],
        ";Parent=", tx$transcript_id[[i]],
        ";gene_id=", tx$gene_id[[i]],
        ";gene_name=", tx$gene_symbol[[i]],
        ";transcript_id=", tx$transcript_id[[i]],
        ";transcript_name=", tx$transcript_name[[i]],
        ";exon_id=", tx$exon_id[[i]],
        ";rank=", tx$exon_rank[[i]]
      )
      writeLines(paste(tx$chr[[i]], "EnsemblREST", "exon", tx$start[[i]], tx$end[[i]], ".", tx$strand[[i]], ".", attrs_exon, sep = "\t"), con)
    }
  }
  invisible(path)
}

message("Fetching Ensembl release metadata")
release_json <- read_json_url(
  paste0(ensembl_base, "/info/software?content-type=application/json"),
  file.path(raw_dir, "ensembl_release.json")
)
ensembl_release <- as.character(release_json$release)
species$ensembl_release <- ensembl_release

parsed <- list()
for (i in seq_len(nrow(species))) {
  row <- species[i, , drop = FALSE]
  message("Fetching Ensembl gene model for ", row$species, " ", row$gene_symbol)
  url <- paste0(
    ensembl_base, "/lookup/id/", row$ensembl_gene_id,
    "?expand=1;content-type=application/json"
  )
  gene_json <- read_json_url(url, file.path(raw_dir, paste0(row$species, "_", row$gene_symbol, "_lookup.json")))
  parsed[[row$species]] <- parse_ensembl_gene(gene_json, row)
}

genes <- dplyr::bind_rows(lapply(parsed, `[[`, "gene"))
transcripts <- dplyr::bind_rows(lapply(parsed, `[[`, "transcripts"))
exons <- dplyr::bind_rows(lapply(parsed, `[[`, "exons"))
unique_exons <- make_unique_exons(exons)

selected <- dplyr::bind_rows(lapply(split(transcripts, transcripts$species), function(tx) {
  species_exons <- exons[exons$species == tx$species[[1L]], , drop = FALSE]
  anchors <- selection_anchors[selection_anchors$species == tx$species[[1L]], , drop = FALSE]
  select_isoforms_one_species(tx, species_exons, anchors = anchors, n = 4L)
}))

selected_exons <- make_plot_exons(selected, exons, genes, species)
common_exons <- make_common_exons(selected, exons)
common_flags <- common_exons |>
  dplyr::select(
    "species", "exon_id", "isoforms_with_exon", "selected_isoform_count",
    "present_in_all_selected_isoforms"
  )
selected_exons <- selected_exons |>
  dplyr::left_join(common_flags, by = c("species", "exon_id")) |>
  dplyr::mutate(
    exon_role = ifelse(.data$present_in_all_selected_isoforms, "common", "variable")
  )
selected_unique_exons <- make_unique_exons(
  selected_exons |>
    dplyr::select(
      "species", "gene_id", "gene_symbol", "transcript_id", "transcript_name",
      transcript_biotype = "transcript_biotype", "is_canonical", "exon_rank",
      "exon_id", "seqname", "start", "end", "strand"
    )
)
selected_unique_exons <- selected_unique_exons |>
  dplyr::left_join(common_flags, by = c("species", "exon_id")) |>
  dplyr::mutate(
    exon_role = ifelse(.data$present_in_all_selected_isoforms, "common", "variable")
  )

windows <- dplyr::bind_rows(lapply(seq_len(nrow(species)), function(i) {
  row <- species[i, , drop = FALSE]
  gene_row <- genes[genes$species == row$species, , drop = FALSE]
  fetch_ucsc_sequence(row, gene_row, window_padding)
}))

alignment <- run_lastz(windows)
nuclinks <- make_nuclinks(alignment$paf, windows)
exon_homology <- make_exon_homology_candidates(nuclinks, selected_unique_exons)
exon_homology_ranked <- rank_exon_homology_candidates(exon_homology)
if (nrow(exon_homology_ranked) > 0L) {
  exon_homology_ranked <- exon_homology_ranked |>
    dplyr::left_join(
      selected_unique_exons |>
        dplyr::filter(.data$species == "human") |>
        dplyr::select(
          human_exon_id = "exon_id",
          human_exon_role = "exon_role",
          human_common = "present_in_all_selected_isoforms"
        ),
      by = "human_exon_id"
    ) |>
    dplyr::left_join(
      selected_unique_exons |>
        dplyr::filter(.data$species == "mouse") |>
        dplyr::select(
          mouse_exon_id = "exon_id",
          mouse_exon_role = "exon_role",
          mouse_common = "present_in_all_selected_isoforms"
        ),
      by = "mouse_exon_id"
    )
}

genes_meta <- genes |>
  dplyr::select(
    "species",
    "seqname",
    gene_start = "start",
    gene_end = "end",
    gene_strand = "strand",
    gene_length_bp = "length_bp"
  )
windows_meta <- windows |>
  dplyr::rename(window_length_bp = "length_bp")
species_out <- species |>
  dplyr::left_join(genes_meta, by = "species") |>
  dplyr::left_join(windows_meta, by = c("species", "ucsc_genome"))

readr::write_tsv(species_out, file.path(out_dir, "cd44_species.tsv"))
readr::write_tsv(genes, file.path(out_dir, "cd44_genes.tsv"))
readr::write_tsv(transcripts, file.path(out_dir, "cd44_transcripts.tsv"))
readr::write_tsv(exons, file.path(out_dir, "cd44_exons_by_transcript.tsv"))
readr::write_tsv(unique_exons, file.path(out_dir, "cd44_unique_exons.tsv"))
readr::write_tsv(selected, file.path(out_dir, "cd44_selected_isoforms.tsv"))
readr::write_tsv(selected_exons, file.path(out_dir, "cd44_selected_exons.tsv"))
readr::write_tsv(selected_unique_exons, file.path(out_dir, "cd44_selected_unique_exons.tsv"))
readr::write_tsv(common_exons, file.path(out_dir, "cd44_common_exons.tsv"))
readr::write_tsv(nuclinks, file.path(out_dir, "cd44_nuclinks_lastz.tsv"))
readr::write_tsv(exon_homology, file.path(out_dir, "cd44_exon_homology_candidates.tsv"))
readr::write_tsv(exon_homology_ranked, file.path(out_dir, "cd44_exon_homology_ranked.tsv"))

write_selected_gff3(selected_exons, "human", file.path(out_dir, "annotations", "human.gff3"))
write_selected_gff3(selected_exons, "mouse", file.path(out_dir, "annotations", "mouse.gff3"))

provenance <- data.frame(
  key = c(
    "ensembl_release",
    "ensembl_human_lookup",
    "ensembl_mouse_lookup",
    "ensembl_homology",
    "ucsc_human_sequence",
    "ucsc_mouse_sequence",
    "lastz_binary",
    "lastz_command",
    "rscript"
  ),
  value = c(
    ensembl_release,
    paste0(ensembl_base, "/lookup/id/ENSG00000026508?expand=1;content-type=application/json"),
    paste0(ensembl_base, "/lookup/id/ENSMUSG00000005087?expand=1;content-type=application/json"),
    paste0(ensembl_base, "/homology/id/human/ENSG00000026508?target_species=mouse;type=orthologues;content-type=application/json"),
    paste0(ucsc_base, "/getData/sequence?genome=hg38;chrom=chr11;start=", windows$window_start0[windows$species == "human"], ";end=", windows$window_end0[windows$species == "human"]),
    paste0(ucsc_base, "/getData/sequence?genome=mm39;chrom=chr2;start=", windows$window_start0[windows$species == "mouse"], ";end=", windows$window_end0[windows$species == "mouse"]),
    lastz_bin,
    alignment$command %||% NA_character_,
    rscript_bin
  ),
  stringsAsFactors = FALSE
)
readr::write_tsv(provenance, file.path(out_dir, "cd44_provenance.tsv"))

readme <- c(
  "# CD44/Cd44 Pairwise Isoform Demo Candidate",
  "",
  paste0("Generated from Ensembl REST release ", ensembl_release, " and UCSC sequence API."),
  "",
  "This dataset supports a pairwise ggexon tutorial with two annotation tracks",
  "and one middle `geom_nuclink()` panel. The selected isoforms are chosen by",
  "a reproducible rule: keep the Ensembl canonical protein-coding transcript,",
  "add the fewest-exon and most-exon full-locus protein-coding transcripts,",
  "then add the remaining transcript that maximizes exon-set Jaccard distance.",
  "",
  "Key files:",
  "",
  "- `cd44_selected_isoforms.tsv`: representative isoforms selected for plotting.",
  "- `cd44_selected_exons.tsv`: plot-ready exon intervals for those isoforms.",
  "- `cd44_selected_unique_exons.tsv`: unique exon intervals in the selected isoforms.",
  "- `cd44_common_exons.tsv`: exons present in all selected isoforms per species.",
  "- `cd44_nuclinks_lastz.tsv`: LASTZ-derived genomic interval links.",
  "- `cd44_exon_homology_candidates.tsv`: exon-homology candidates from overlaps",
  "  between LASTZ blocks and Ensembl exons.",
  "- `cd44_exon_homology_ranked.tsv`: one row per exon-pair candidate with",
  "  reciprocal-best ranks and common/variable exon flags.",
  "- `annotations/*.gff3`: compact selected-transcript GFF3 files.",
  "- `sequences/*.fa`: genomic DNA windows used for LASTZ.",
  "- `cd44_provenance.tsv`: source URLs and local command provenance."
)
writeLines(readme, file.path(out_dir, "README.md"))

message("Wrote CD44 demo candidate files to ", out_dir)
message("Selected isoforms:")
print(selected[, c("species", "transcript_name", "transcript_id", "exon_count", "selection_reason")], row.names = FALSE)
message("LASTZ links retained: ", nrow(nuclinks))
message("Exon homology candidate rows: ", nrow(exon_homology))
