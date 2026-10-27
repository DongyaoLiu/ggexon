# Example helpers, deliberately outside the exported package API.
load_dox_example <- function(demo_dir = system.file("extdata", "dox_synteny", package = "ggexon")) {
  stopifnot(dir.exists(demo_dir))
  read_table <- function(name) utils::read.delim(file.path(demo_dir, name), check.names = FALSE)
  windows <- read_table("windows.tsv")
  genes <- read_table("genes.tsv")
  features <- read_table("features.tsv")
  coverage <- read_table("coverage.tsv.gz")
  links <- read_table("links.tsv")
  track_levels <- c("Dmau", "link_Dmau_Dsim", "Dsim", "link_Dsim_Dsech", "Dsech")
  genes$track <- factor(genes$species, levels = track_levels)
  coverage$track <- factor(coverage$species, levels = track_levels)
  coverage$signal <- abs(coverage$score)
  coverage$signal_class <- ifelse(coverage$strand == "+", "RNA +", "RNA -")
  links$track <- factor(paste0("link_", links$tspecies, "_", links$qspecies), levels = track_levels)
  links$identity_band <- cut(links$identity, c(85, 95, 98, 100),
    include.lowest = TRUE, right = TRUE, labels = c("85-95%", "95-98%", "98-100%"))

  # Split the mapped exon spans at CDS boundaries. These are source-derived
  # UTR/CDS pieces, not inferred exon boundaries from a coverage threshold.
  exon_rows <- features[features$type == "exon", ]
  pieces <- lapply(seq_len(nrow(exon_rows)), function(i) {
    e <- exon_rows[i, ]
    cds <- features[features$transcript_id == e$transcript_id & features$type == "CDS" &
      features$exon_number == e$exon_number, ]
    bounds <- c(e$feature_start, e$feature_end + 1L)
    if (nrow(cds)) bounds <- sort(unique(c(bounds, cds$feature_start, cds$feature_end + 1L)))
    do.call(rbind, lapply(seq_len(length(bounds) - 1L), function(j) {
      a <- bounds[j]
      b <- bounds[j + 1L] - 1L
      data.frame(species = e$species, track = factor(e$species, levels = track_levels),
        seqnames = "X", xmin = a, xmax = b, ymin = 1,
        transcripts = e$transcript_id, gene_name = e$gene_name, strand = e$strand,
        type = if (nrow(cds) && any(cds$feature_start <= a & cds$feature_end >= b)) "CDS" else "UTR")
    }))
  })
  exons <- do.call(rbind, pieces)
  # Boundary points keep the full contiguous source windows visible.
  limits <- do.call(rbind, lapply(seq_len(nrow(windows)), function(i)
    data.frame(track = factor(windows$species[i], levels = track_levels),
      x = c(windows$start[i] - .5, windows$end[i] + .5),
      y = c(.5, 2.1), .ggexon_panel_role = "annotation")))
  panels <- data.frame(PANEL = 1:8, ROW = 1:8, COL = 1L,
    track = c("Dmau", "Dmau", "link_Dmau_Dsim", "Dsim", "Dsim",
      "link_Dsim_Dsech", "Dsech", "Dsech"),
    panel_type = c("coverage", "annotation", "link", "coverage", "annotation",
      "link", "coverage", "annotation"),
    tspecies = c(NA, NA, "Dmau", NA, NA, "Dsim", NA, NA),
    qspecies = c(NA, NA, "Dsim", NA, NA, "Dsech", NA, NA))
  wi <- match(panels$track, windows$species)
  panels$species <- windows$species[wi]
  panels$xlim_chr <- ifelse(is.na(wi), NA_character_, "X")
  panels$xlim_min <- windows$start[wi] - .5
  panels$xlim_max <- windows$end[wi] + .5
  list(windows = windows, genes = genes, exons = exons, coverage = coverage,
    links = links, limits = limits, panels = panels, demo_dir = demo_dir,
    manifest = jsonlite::fromJSON(file.path(demo_dir, "manifest.json")))
}

plot_dox_example <- function(example = load_dox_example(), intron_factor = 1,
                             base_size = 9, base_family = "sans",
                             coverage_panel_height = .5, link_panel_height = .5) {
  stopifnot(length(intron_factor) == 1L, is.finite(intron_factor), intron_factor > 0)
  palette <- c("CG42797" = "#246589", "Ptpmeg2" = "#246589", "Dox" = "#D55E00",
    "RNA +" = "#749CB5", "RNA -" = "#67AD91",
    "85-95%" = "#E2E7ED", "95-98%" = "#A4BCD0", "98-100%" = "#497D9F")
  labels <- example$genes
  labels$x <- (labels$start + labels$end) / 2
  labels$y <- 1.8
  labels$label <- labels$gene_name
  species_ids <- as.character(example$windows$species)
  coverage_limits <- stats::setNames(lapply(species_ids, function(id)
    c(0, max(example$coverage$signal[example$coverage$species == id]) * 1.1)),
    species_ids)
  peaks <- do.call(rbind, lapply(species_ids, function(id) {
    w <- example$windows[example$windows$species == id, ]
    data.frame(track = factor(id, levels = levels(example$genes$track)),
      x = w$end, y = coverage_limits[[id]][[2L]] * .96,
      label = paste0("max ", round(max(example$coverage$signal[example$coverage$species == id]), 1)),
      .ggexon_panel_role = "coverage")
  }))
  species <- ggexon::SynSpecies(name = "Dox insertion locus")
  ggexon::species_layout(species) <- ggexon::SynLayout(
    panels = example$panels, free = list(x = TRUE, y = TRUE))
  size_mm <- base_size / (72.27 / 25.4)
  exon_layer <- function(data, backbone_colour) {
    ggexon::geom_exon2(data = data,
      ggplot2::aes(xmin = xmin - .5, xmax = xmax + .5, ymin = ymin,
        transcripts = transcripts, strand = strand, track = track, type = type,
        fill = gene_name), annotation_type = "all", exon_height = .48,
      compress_introns = FALSE, utr_height = .45, cds_height = 1,
      intron_shape = "chevron", intron_peak = .22, arrow_width = 80,
      transcript_backbone_colour = backbone_colour, colour = NA, linewidth = .25,
      show.legend = FALSE, inherit.aes = FALSE)
  }
  p <- ggexon::ggexon(species) +
    ggplot2::geom_blank(data = example$limits, ggplot2::aes(x = x, y = y)) +
    ggexon::geom_coverage(data = example$coverage,
      ggplot2::aes(xmin = start, xmax = end, coverage = signal, track = track,
        fill = signal_class), inherit.aes = FALSE, alpha = .85) +
    exon_layer(example$exons[example$exons$gene_name != "Dox", ], "#4D6A7A") +
    exon_layer(example$exons[example$exons$gene_name == "Dox", ], unname(palette["Dox"])) +
    ggplot2::geom_text(data = labels,
      ggplot2::aes(x = x, y = y, label = label, colour = gene_name),
      size = size_mm, family = base_family, fontface = "italic", inherit.aes = FALSE,
      show.legend = FALSE) +
    ggplot2::geom_text(data = peaks, ggplot2::aes(x = x, y = y, label = label),
      size = size_mm * .85, family = base_family, colour = "grey45", hjust = 1,
      vjust = 1, inherit.aes = FALSE) +
    ggexon::geom_nuclink(data = example$links,
      ggplot2::aes(tspecies = tspecies, tchr = tchr, tstart = tstart, tend = tend,
        qspecies = qspecies, qchr = qchr, qstart = qstart, qend = qend,
        strand = strand, group = group, fill = identity_band),
      alpha = .55, colour = NA, inherit.aes = FALSE) +
    ggexon::facet_genomics(ggplot2::vars(track), ncol = 1, scales = "free",
      strip.position = "left", link_panel_height = link_panel_height,
      coverage_panel_height = coverage_panel_height,
      link_axis = "none", link_strip = "blank", annotation_axis = "all",
      labeller = ggplot2::as_labeller(c(Dmau = "D. mauritiana", Dsim = "D. simulans",
        Dsech = "D. sechellia", link_Dmau_Dsim = "", link_Dsim_Dsech = ""))) +
    ggexon::scale_panel_coverage("free_y", limits = coverage_limits) +
    ggplot2::scale_fill_manual(values = palette,
      breaks = c("RNA +", "RNA -", "85-95%", "95-98%", "98-100%"),
      labels = c("RNA-seq +", "RNA-seq - (magnitude)", "DNA 85-95%", "DNA 95-98%", "DNA 98-100%"),
      name = NULL) +
    ggplot2::scale_colour_manual(values = palette, guide = "none") +
    ggexon::scale_x_ggexon_genomic(intron_factor = intron_factor,
      n.breaks = 4, labels = scales::label_number(scale = 1e-6, suffix = " Mb", accuracy = .01)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggexon::theme_ggexon_track(base_size = base_size, base_family = base_family,
      show_x_axis = TRUE, show_y_axis = FALSE, show_x_grid = FALSE,
      show_legend = TRUE) +
    ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0, face = "italic"),
      strip.background = ggplot2::element_blank(), panel.border = ggplot2::element_blank(),
      legend.position = "bottom", legend.text = ggplot2::element_text(size = base_size),
      legend.key.width = grid::unit(4, "mm"),
      panel.spacing.y = grid::unit(.3, "lines"),
      plot.margin = ggplot2::margin(8, 12, 8, 8))
  list(plot = p, example = example, intron_factor = intron_factor)
}
