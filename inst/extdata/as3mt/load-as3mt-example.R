# Example-level helpers; not exported package functions.
load_as3mt_example <- function(demo_dir = system.file("extdata", "as3mt", package = "ggexon")) {
  stopifnot(dir.exists(demo_dir))
  read_table <- function(name) utils::read.delim(file.path(demo_dir, name),
    check.names = FALSE, fileEncoding = "UTF-8")
  specification <- readRDS(file.path(demo_dir, "specification.rds"))
  links <- read_table("human-chimp-links.nogap.tsv")
  levels <- unique(links$identity_band)
  levels <- levels[order(as.numeric(sub("–.*", "", levels)))]
  links$identity_band <- factor(links$identity_band, levels = levels)
  selected <- links[links$identity_band %in% tail(levels, 2L), , drop = FALSE]
  exons <- do.call(rbind, lapply(seq_along(specification$transcript_models), function(i) {
    model <- specification$transcript_models[[i]]
    do.call(rbind, lapply(model$exons, function(e) data.frame(
      id = c("human_full", "human_short", "chimp")[i],
      exon = e$exon_number_in_full_reference,
      start = e$start_1based, end = e$end_1based)))
  }))
  comparison <- ggexon::SynSpecies(name = "AS3MT human-chimpanzee transcript comparison",
    metadata = list(specification = specification, all_ungapped_links = links,
      alignment_provenance = readRDS(file.path(demo_dir, "alignment-provenance.rds"))))
  for (species in c("human", "chimp")) {
    individual <- ggexon::load_annotation(ggexon::SynIndividual(id = species,
      annotation_file = file.path(demo_dir, paste0(species, ".gff3"))))
    features <- ggexon::annotation_data(individual)
    md <- S4Vectors::mcols(features)
    md$model <- as.character(md$transcript_id)
    md$isoform_y <- ifelse(md$model == "human_full", 1.8, .7)
    md$exon_class <- ifelse(md$model == "human_full" & md$exon_number %in% c(2, 3),
      "skipped_exons", md$model)
    S4Vectors::mcols(features) <- md
    ggexon::annotation_data(individual) <- features
    comparison <- ggexon::add_individual(comparison, individual)
  }
  alignment <- ggexon::SynPairAlignment(name = "human_chimp", target_individual = "human",
    query_individual = "chimp", file = "<AS3MT ungapped genomic links>",
    format = "paf", data = selected,
    metadata = list(coordinate_system = "one-based closed chromosome coordinates",
      alignment_mode = "ungapped LASTZ blocks with chaining selection"))
  comparison <- ggexon::add_pairwise_alignment(comparison, ggexon::load_alignment(alignment))
  comparison <- ggexon::store_chain_layout(comparison, free = list(x = TRUE, y = FALSE))
  comparison <- ggexon::set_panel_xlim(comparison, individual = c("human", "chimp"),
    xlim = list(human = c(102869400, 102902000), chimp = c(104324090, 104356300)),
    xlim_chr = c(human = "chr10", chimp = "chr10"))
  methods::validObject(comparison)
  list(comparison = comparison, specification = specification, exons = exons,
    links = links, selected_links = selected,
    organoid_values = read_table("organoid-reported-values.tsv"),
    organoid_summary = read_table("organoid-summary.tsv"))
}

plot_as3mt_example <- function(example, intron_factor = 30, base_size = 8, base_family = "serif") {
  stopifnot(is.numeric(intron_factor), length(intron_factor) == 1L, intron_factor > 0)
  palette <- c(human_full = "#0072B2", human_short = "#D55E00", chimp = "#64748B",
    skipped_exons = "#E69F00")
  bands <- levels(example$links$identity_band)
  palette <- c(palette, stats::setNames(scales::pal_viridis()(length(bands)), bands))
  lab <- example$exons
  lab$track <- ifelse(lab$id == "chimp", "chimp", "human")
  lab$x <- (lab$start + lab$end) / 2
  lab$y <- ifelse(lab$id == "human_full", 2.45, 1.35)
  lab$y[lab$exon == 2] <- lab$y[lab$exon == 2] + .25
  lab$label <- paste0("E", lab$exon)
  ghosts <- lab[lab$id == "human_full" & lab$exon %in% c(2, 3), ]
  junction <- data.frame(track = "human", x = 102870950, y = .35, label = "E1 → E4")
  text_mm <- base_size / (72.27 / 25.4)
  plot <- ggexon::ggexon(example$comparison) +
    ggexon::geom_exon2(mapping = ggplot2::aes(xmin = xmin - .5, xmax = xmax + .5,
      ymin = isoform_y, transcripts = transcript_name, fill = exon_class),
      annotation_type = "all", exon_height = .42, compress_introns = FALSE,
      utr_height = .35, cds_height = 1, intron_shape = "chevron", arrow_width = 1e-8,
      colour = NA, transcript_backbone_colour = "grey40") +
    ggplot2::geom_text(data = lab, ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE, family = base_family, size = text_mm) +
    ggplot2::geom_rect(data = ghosts, ggplot2::aes(xmin = start - .5, xmax = end + .5,
      ymin = .6, ymax = 1), inherit.aes = FALSE, fill = NA, colour = palette[["human_short"]],
      linetype = "dashed", linewidth = .3) +
    ggplot2::geom_text(data = junction, ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE, family = base_family, size = text_mm, colour = palette[["human_short"]]) +
    ggexon::geom_nuclink(alignment = "human_chimp", mapping = ggplot2::aes(fill = identity_band),
      colour = NA, alpha = .25, inherit.aes = FALSE) +
    ggexon::facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1,
      link_panel_height = .35, link_axis = "none", link_strip = "blank", annotation_axis = "all",
      strip.position = "left", labeller = ggplot2::as_labeller(c(human = "Human\nGRCh38",
        chimp = "Chimpanzee\nPan_tro_3.0", link_human_chimp = ""))) +
    ggplot2::scale_fill_manual(values = palette, breaks = tail(bands, 2L), name = "LASTZ identity (%)") +
    ggexon::scale_x_ggexon_genomic(intron_factor = intron_factor,
      guide = ggexon::guide_x_ggexon_piecewise(by = "panel")) +
    ggplot2::labs(title = "AS3MT transcripts: human–chimpanzee pairwise comparison",
      subtitle = sprintf("Exons at native scale · introns compressed %g-fold · full-reference exon numbers", intron_factor),
      x = NULL, y = NULL,
      caption = paste("Human upper row: full-length; lower row: paper-defined AS3MTd2d3 reconstruction.",
        "Gold: E2/E3 in the full-length human transcript; dashed boxes: skipped in d2d3.",
        "Thick: CDS; thin: UTR. The scale uses the exon union within each assembly panel.",
        "Ungapped LASTZ DNA blocks: only the two highest identity categories shown; no within-block indels.",
        "Links split at exon/intron scale boundaries automatically; ribbons do not measure expression.",
        "Li et al. 2016: doi:10.1038/nm.4096 | Kim et al. 2025: doi:10.1126/sciadv.adp8271", sep = "\n")) +
    ggexon::theme_ggexon_track(base_size = base_size, base_family = base_family,
      show_x_axis = TRUE, show_x_grid = FALSE, show_legend = TRUE) +
    ggplot2::theme(text = ggplot2::element_text(size = base_size),
      plot.title = ggplot2::element_text(size = base_size, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size),
      plot.caption = ggplot2::element_text(size = base_size, hjust = 0),
      strip.text = ggplot2::element_text(size = base_size),
      strip.text.y.left = ggplot2::element_text(size = base_size, angle = 0),
      axis.text = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size),
      legend.title = ggplot2::element_text(size = base_size),
      legend.position = "bottom", panel.spacing.y = grid::unit(.35, "lines"))
  list(plot = plot, build = ggexon::ggexon_build(plot))
}
