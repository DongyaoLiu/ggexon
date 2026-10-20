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

# Project the assigned protein start through the full-length reference CDS.
# This is an isoform consequence, not a substitution or a causal DNA variant.
as3mt_protein_change <- function(example) {
  assigned <- example$specification$reference_validation$paper_defined_d2d3_reconstruction
  event <- data.frame(gene_id = "AS3MT", position = assigned$lost_N_terminal_residues + 1L,
    mutation = "d2d3 assigned start", lost_aa = assigned$lost_N_terminal_residues)
  human <- suppressMessages(ggexon::subset_individual(example$comparison,
    individual = "human", chr = "chr10"))
  human <- ggexon::add_annotation(human, ggexon::SynProteinMutationAnnotation(
    name = "d2d3_start", mutation_file = "<paper-defined protein start>",
    mutation_data = event, keytype = "gene_id", lazy = FALSE), set_active = FALSE)
  projected <- ggexon::project_mutations_to_genome(human, annotation = "d2d3_start",
    transcripts = "human_full")
  stopifnot(nrow(projected) == 1L,
    identical(as.numeric(c(projected$xmin, projected$xmax)),
      as.numeric(assigned$translation_start_genomic_1based)))
  projected$anchor <- (projected$xmin + projected$xmax) / 2
  projected$track <- "human"
  projected$protein_event <- "isoform_start"
  projected$label <- sprintf("Start: M%d \u2192 M1", projected$position)
  projected
}

# Use the existing interval projector for the absent N-terminal protein region.
# This annotation is a protein interval, not a functional-domain claim.
as3mt_protein_loss <- function(example) {
  lost <- example$specification$reference_validation$paper_defined_d2d3_reconstruction$lost_N_terminal_residues
  human <- suppressMessages(ggexon::subset_individual(example$comparison,
    individual = "human", chr = "chr10"))
  interval <- methods::new("SynProteinDomainAnnotation", name = "d2d3_absent",
    source_file = "<paper-defined absent protein interval>", keytype = "gene_id",
    loaded = TRUE, lazy = FALSE, domain_data = S4Vectors::DataFrame(
      gene_id = "AS3MT", domain = "Absent in d2d3", start = 1L, end = lost))
  human <- ggexon::add_annotation(human, interval, set_active = FALSE)
  projected <- ggexon::project_domains_to_genome(human, annotation = "d2d3_absent",
    transcripts = "human_full")
  projected <- projected[order(projected$xmin), , drop = FALSE]
  stopifnot(sum(projected$xmax - projected$xmin + 1) == 3 * lost)
  projected$track <- "human"
  projected$protein_event <- "lost_residues"
  projected
}

as3mt_protein_layers <- function(change, loss, base_size = 8, base_family = "serif") {
  text_size <- base_size / (72.27 / 25.4)
  lost_label <- loss[1, , drop = FALSE]
  lost_label$label <- sprintf("aa1\u2013%d lost", change$lost_aa)
  list(
    ggplot2::geom_rect(data = loss, ggplot2::aes(xmin = xmin - .5, xmax = xmax + .5,
      ymin = 1.70, ymax = 1.77), fill = "#D55E00", colour = NA, inherit.aes = FALSE),
    ggplot2::geom_segment(data = change, ggplot2::aes(x = anchor, xend = anchor),
      y = 1.74, yend = 2.28, colour = "#D55E00", linewidth = .55, inherit.aes = FALSE),
    ggplot2::geom_text(data = lost_label, ggplot2::aes(x = xmin, label = label),
      y = 1.53, hjust = 0, family = base_family, size = text_size,
      colour = "#D55E00", inherit.aes = FALSE),
    ggplot2::geom_text(data = change, ggplot2::aes(x = anchor, label = label),
      y = 1.53, hjust = -.07, family = base_family, size = text_size,
      colour = "#D55E00", inherit.aes = FALSE))
}

plot_as3mt_example <- function(example, intron_factor = 30, base_size = 8, base_family = "serif",
                               link_data = NULL, show_protein = TRUE) {
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
    ggexon::geom_nuclink(alignment = "human_chimp", data = link_data,
      mapping = ggplot2::aes(fill = identity_band, tspecies = tspecies, tchr = tchr,
        tstart = tstart, tend = tend, qspecies = qspecies, qchr = qchr,
        qstart = qstart, qend = qend, strand = strand, group = group),
      colour = NA, alpha = .25, inherit.aes = FALSE) +
    ggexon::facet_genomics(ggplot2::vars(track), scales = "free_x", ncol = 1,
      link_panel_height = .35, link_axis = "none", link_strip = "blank", annotation_axis = "all",
      strip.position = "left", labeller = ggplot2::as_labeller(c(human = "Human\nGRCh38",
        chimp = "Chimpanzee\nPan_tro_3.0", link_human_chimp = ""))) +
    ggplot2::scale_fill_manual(values = palette, breaks = tail(bands, 2L), name = "LASTZ identity (%)") +
    ggexon::scale_x_ggexon_genomic(intron_factor = intron_factor,
      guide = ggexon::guide_x_ggexon_piecewise(by = "panel")) +
    ggplot2::labs(title = "AS3MT transcripts: human–chimpanzee pairwise comparison",
      subtitle = NULL, x = NULL, y = NULL,
      caption = paste("Gold: skipped E2/E3. Thick boxes: CDS; thin boxes: UTR.",
        "Lower human row: paper-defined d2d3 reconstruction.", sep = "\n")) +
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
  change <- as3mt_protein_change(example)
  loss <- as3mt_protein_loss(example)
  if (isTRUE(show_protein)) plot <- plot + as3mt_protein_layers(change, loss, base_size, base_family)
  list(plot = plot, build = ggexon::ggexon_build(plot), protein_change = change, protein_loss = loss)
}

# Explicit DNA differences; the overview's ungapped ribbons are not variant calls.
plot_as3mt_alignseq <- function(demo_dir = system.file("extdata", "as3mt", package = "ggexon"),
                               intron_factor = 30, base_size = 8, base_family = "serif") {
  example <- load_as3mt_example(demo_dir)
  a <- readRDS(file.path(demo_dir, "vntr-dna.rds"))
  d <- ggexon::alignseq_data(a$aligned, a$anchors, reference = "human",
    regions = a$regions, details = a$details, flank = a$flank)
  d$y <- ifelse(d$sequence == "human", 2.65, 1.5)
  d$boundary_after <- !is.na(d$position) & d$position == a$donor_after[d$sequence]
  d$boundary_label <- ifelse(d$boundary_after, "E1 donor | intron 1", "")
  # Mark the copy-number difference; two base differences in the chimp homolog
  # remain visible as sequence context, not as claimed splicing-causal SNVs.
  d$event_id[d$event_type == "substitution"] <- NA_integer_
  repeats <- rbind(
    data.frame(track = "human", start = a$human_units$start, end = a$human_units$end, y = 1.8),
    data.frame(track = "human", start = a$human_units$start, end = a$human_units$end, y = .7),
    data.frame(track = "chimp", start = a$chimp_unit_start, end = a$chimp_unit_end, y = .7))
  donors <- data.frame(track = c("human", "chimp"),
    x = unname(a$donor_after) + .5, y = c(1.48, .38), yend = c(2.12, 1.02))
  # Equal physical bp scales across assembly panels allow one shared pair of bars.
  initial <- plot_as3mt_example(example, intron_factor, base_size, base_family)$build
  windows <- list(human = c(102869400, 102902000), chimp = c(104324090, 104356300))
  layout <- initial$layout$layout
  spans <- vapply(names(windows), function(species) {
    panel <- as.integer(layout$PANEL[layout$track == species])
    tr <- initial$layout$genomic_x_transforms[[as.character(panel)]]
    diff(range(tr$plot_start, tr$plot_end)) +
      min(tr$genomic_start) - windows[[species]][1] +
      windows[[species]][2] - max(tr$genomic_end)
  }, numeric(1))
  for (species in names(windows)) {
    pad <- (max(spans) - spans[[species]]) / 2
    windows[[species]] <- windows[[species]] + c(-pad, pad)
  }
  example$comparison <- ggexon::set_panel_xlim(example$comparison,
    individual = names(windows), xlim = windows,
    xlim_chr = c(human = "chr10", chimp = "chr10"))
  plot <- plot_as3mt_example(example, intron_factor, base_size, base_family,
    # Use the same raw genomic links; fractional padding affects display only.
    link_data = example$selected_links)$plot +
    ggexon::geom_alignseq(data = d, size = base_size / (72.27 / 25.4), family = base_family) +
    ggexon::scale_x_ggexon_genomic(intron_factor = intron_factor, guide = "none") +
    ggexon::scale_panel_annotation("free_y",
      limits = list(human = c(.15, 3.95), chimp = c(.2, 2.8))) +
    ggplot2::geom_rect(data = repeats, ggplot2::aes(xmin = start - .5, xmax = end + .5,
      ymin = y + .08, ymax = y + .34), inherit.aes = FALSE,
      fill = NA, colour = "#CC79A7", linewidth = .55) +
    ggplot2::geom_segment(data = donors,
      ggplot2::aes(x = x, xend = x, y = y, yend = yend), inherit.aes = FALSE,
      colour = "grey20", linewidth = .35) +
    ggplot2::labs(title = NULL, subtitle = NULL,
      caption = paste("Exon-1 VNTR (purple). \u221272 bp: chimp relative to human.",
        "Lower human row: paper-defined d2d3 reconstruction. Cai et al. (2021).", sep = "\n"))
  short <- ggexon::alignseq_data(a$aligned, a$anchors, reference = "human", regions = a$details)
  short$track <- factor(short$track, levels = c("human", "chimp"))
  short$y <- .75
  short$boundary_after <- !is.na(short$position) & short$position == a$donor_after[short$sequence]
  short$boundary_label <- ifelse(short$boundary_after, "E1 donor | intron 1", "")
  backbone <- do.call(rbind, lapply(split(short, short$sequence), function(d)
    data.frame(x = min(d$x) - 1, xend = max(d$x) + 1, y = .4, yend = .4,
      track = d$track[1])))
  exon_parts <- data.frame(track = c("human", "chimp"),
    start = c(102869570, a$chimp_unit_start), end = unname(a$donor_after))
  exon_parts$track <- factor(exon_parts$track, levels = c("human", "chimp"))
  backbone$track <- factor(backbone$track, levels = c("human", "chimp"))
  rows <- ggexon::ggexon(backbone) +
    ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      colour = "grey50", linewidth = .4) +
    ggplot2::geom_rect(data = exon_parts,
      ggplot2::aes(xmin = start - .5, xmax = end + .5, ymin = .3, ymax = .5),
      inherit.aes = FALSE, fill = "grey65", colour = NA) +
    ggexon::geom_alignseq(data = short, mode = "rows", size = base_size / (72.27 / 25.4), family = base_family) +
    ggexon::facet_genomics(ggplot2::vars(track), ncol = 1, scales = "free_x") +
    ggplot2::labs(x = "Assembly-specific chr10 genomic coordinates", y = NULL,
      caption = "Terminal VNTR unit: E1 (24 nt) | intron 1 (12 nt).") +
    ggexon::theme_ggexon_track(base_size = base_size, base_family = base_family,
      show_x_axis = TRUE, show_x_grid = FALSE)
  display <- as3mt_sequence_legend(plot, intron_factor, base_size, base_family)
  list(plot = plot, rows = rows, data = d, alignment = a,
    grob = display$grob, scale_bars = display$scale_bars, display_spans = display$display_spans, vntr = a)
}


# Place exactly two bars in a separate legend row, calibrated to the annotation
# panel width. Both assemblies have the same display span, including expansion.
as3mt_sequence_legend <- function(plot, intron_factor, base_size, base_family) {
  built <- ggexon::ggexon_build(plot)
  layout <- built$layout$layout
  ids <- as.integer(layout$PANEL[match(c("human", "chimp"), layout$track)])
  spans <- vapply(ids, function(i) diff(built$layout$panel_params[[i]]$x.range), numeric(1))
  stopifnot(abs(diff(spans)) < 1e-5)
  bars <- data.frame(region_type = c("exon", "intron"),
    genomic_width = c(500, 500 * intron_factor), plot_width = 500,
    fraction = 500 / spans[1])
  table <- ggplot2::ggplot_gtable(built)
  panel_rows <- which(grepl("^panel-", table$layout$name))
  for (species in c("human", "chimp")) {
    row <- layout$ROW[layout$track == species]
    col <- layout$COL[layout$track == species]
    index <- which(table$layout$name == paste0("panel-", col, "-", row))
    if (length(index) == 1) table$heights[table$layout$t[index]] <-
      grid::unit(if (species == "human") 3.5 else 2.3, "null")
  }
  gp <- grid::gpar(fontfamily = base_family, fontsize = base_size, col = "grey20")
  scale_grobs <- lapply(1:2, function(i) {
    left <- c(.16, .62)[i]
    right <- left + bars$fraction[i]
    text <- if (i == 1) "Exon 500 bp" else
      sprintf("Intron %g kb (\u00f7%g)", bars$genomic_width[i] / 1000, intron_factor)
    grid::gTree(name = paste0(bars$region_type[i], "-scale-bar"), children = grid::gList(
      grid::segmentsGrob(x0 = left, x1 = right, y0 = .25, y1 = .25,
        gp = grid::gpar(col = "grey20", lwd = .8)),
      grid::segmentsGrob(x0 = c(left, right), x1 = c(left, right), y0 = .15, y1 = .35,
        gp = grid::gpar(col = "grey20", lwd = .8)),
      grid::textGrob(text, x = (left + right) / 2, y = .7, gp = gp)))
  })
  legend <- grid::gTree(name = "as3mt-scale-legend", children = do.call(grid::gList, scale_grobs))
  row <- table$layout$t[table$layout$name == "guide-box-bottom"][1]
  if (is.na(row)) row <- max(table$layout$b[panel_rows])
  table <- gtable::gtable_add_rows(table, grid::unit(9, "mm"), pos = row)
  table <- gtable::gtable_add_grob(table, legend, t = row + 1,
    l = min(table$layout$l[panel_rows]), r = max(table$layout$r[panel_rows]),
    name = "as3mt-scale-legend", clip = "off")
  panel_left <- min(table$layout$l[panel_rows]); panel_right <- max(table$layout$r[panel_rows])
  evidence <- list(grid::textGrob(
    "Human minigenes: 3 repeats give more d2d3 than 2",
    x = .5, y = .85, gp = gp))
  for (i in 1:2) {
    copies <- c(2, 3)[i]; left <- c(.27, .72)[i]
    evidence[[length(evidence) + 1L]] <- grid::textGrob(paste0(copies, "-repeat"),
      x = left - .02, y = .33, hjust = 1, gp = gp)
    for (j in seq_len(copies)) {
      centre <- left + (j - .5) * .035
      evidence[[length(evidence) + 1L]] <- grid::rectGrob(x = centre, y = .33,
        width = .033, height = .28, gp = grid::gpar(fill = "#F3D7E6", col = "#CC79A7"))
      evidence[[length(evidence) + 1L]] <- grid::textGrob(paste0("R", j), x = centre, y = .33, gp = gp)
    }
  }
  table <- gtable::gtable_add_rows(table, grid::unit(12, "mm"), pos = row + 1)
  table <- gtable::gtable_add_grob(table,
    grid::gTree(children = do.call(grid::gList, evidence), name = "as3mt-vntr-evidence"),
    t = row + 2, l = panel_left, r = panel_right, name = "as3mt-vntr-evidence", clip = "off")
  list(grob = table, scale_bars = bars, display_spans = spans)
}
