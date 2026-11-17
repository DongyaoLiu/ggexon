# Dense HLA-A summary. All visual controls are collected here.
hla_a_plot_parameters <- list(
  width_mm = 240, height_mm = 135, text_size_pt = 10, family = "Helvetica",
  point_size = 0.65, stem_linewidth = 0.14, stem_alpha = 0.27,
  head_half_span = 1.6, focus_half_span = 3.2, stem_points = 24L,
  wong = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#0072B2"),
  # Paul Tol's Bright qualitative scheme (six region colours).
  region_colours = c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377")
)

# Fan heads locally; a crowded region cannot push later heads off the protein.
# Head heights are log10(count + 1), with the original counts on the tick labels.
hla_a_lollipop_layout <- function(mutations, hla, half_span, parameters,
                                  position = "mature_position", domains = hla_a_regions(hla),
                                  track_length = hla_a_evidence(hla)$protein_length) {
  prepared <- ggexon::protein_lollipop_data(
    mutations, domains = domains, domain = "domain", position = position,
    protein_length = track_length, protein_start = 1,
    mutation_y_by = "allele_count", mutation_y_trans = "log10",
    mutation_y_range = range(log10(mutations$allele_count + 1)),
    spread_threshold = 0, stem_points = parameters$stem_points,
    track_ymin = -0.32, track_ymax = -0.1,
    domain_default_ymin = -0.34, domain_default_ymax = -0.08
  )
  heads <- prepared$mutations
  heads$head_offset <- 0
  for (indices in split(seq_len(nrow(heads)), heads$lollipop_position)) {
    if (length(indices) > 1L) {
      span <- min(half_span, heads$lollipop_position[indices[[1L]]],
                  track_length + 1 - heads$lollipop_position[indices[[1L]]])
      heads$head_offset[indices] <- seq(-span, span, length.out = length(indices))
    }
  }
  heads$lollipop_x <- heads$lollipop_position + heads$head_offset
  t <- rep(seq(0, 1, length.out = parameters$stem_points), nrow(heads))
  curve <- t * t * (3 - 2 * t)
  ids <- prepared$stems$lollipop_curve_id
  prepared$stems$lollipop_x <- heads$lollipop_position[ids] +
    (heads$lollipop_x[ids] - heads$lollipop_position[ids]) * curve
  prepared$stems$amino_acid_group <- heads$amino_acid_group[ids]
  prepared$mutations <- heads
  prepared
}

plot_hla_a_example <- function(hla, parameters = hla_a_plot_parameters) {
  p <- parameters
  evidence <- hla_a_evidence(hla)
  mutations <- ggexon::query_protein_mutations(hla, annotation = "hla_substitutions")
  group_names <- c("Aliphatic", "Aromatic", "Polar", "Basic", "Acidic", "Gly / Pro")
  group_colours <- stats::setNames(p$wong, group_names)
  domain_colours <- stats::setNames(p$region_colours, hla_a_regions(hla)$domain)
  region_labels <- function(model) {
    regions <- hla_a_regions(model)
    regions$midpoint <- (regions$start + regions$end) / 2
    regions$short_label <- c("ALPHA 1", "ALPHA 2", "ALPHA 3", "CP", "TM", "TAIL")
    regions$label_colour <- c("white", "#17212B", "white", "#17212B", "#17212B", "white")
    regions
  }
  count_breaks <- c(1, 10, 100, 1000, 5000)
  text_mm <- p$text_size_pt / ggplot2::.pt
  base_theme <- ggplot2::theme_minimal(base_size = p$text_size_pt, base_family = p$family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E5E9EC", linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = "#5E6C78"),
      axis.title = ggplot2::element_text(colour = "#344854"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      plot.title = ggplot2::element_text(size = 23, face = "bold", colour = "#203747",
                                        margin = ggplot2::margin(b = 7)),
      plot.subtitle = ggplot2::element_text(size = 11, colour = "#5E6C78",
                                           margin = ggplot2::margin(b = 18)),
      plot.caption = ggplot2::element_text(size = 9, hjust = 0, colour = "#697884",
                                          margin = ggplot2::margin(t = 14)),
      plot.title.position = "plot", plot.caption.position = "plot",
      legend.position = "bottom", legend.title = ggplot2::element_text(face = "bold"),
      legend.key.width = grid::unit(9, "mm"),
      plot.margin = ggplot2::margin(16, 18, 12, 14)
    )
  draw <- function(layout) {
    # Every layer reads the same Syn object. Native dispatch resolves the
    # structural track; data functions add this example's bounded local fan.
    ggexon::ggexon(hla) +
      ggexon::geom_protein_lollipop(
        annotation = "hla_substitutions", domain_annotation = "hla_regions",
        position = "mature_position", domain_start = "mature_start", domain_end = "mature_end",
        domain = "domain", protein_length = evidence$protein_length, protein_start = 1,
        track_ymin = -0.32, track_ymax = -0.1,
        domain_default_ymin = -0.34, domain_default_ymax = -0.08,
        show_stems = FALSE, show_points = FALSE, show.legend = FALSE
      ) +
      ggplot2::geom_line(
        data = function(model) layout(model)$stems,
        ggplot2::aes(x = lollipop_x, y = lollipop_y,
          group = lollipop_curve_id, colour = amino_acid_group),
        linewidth = p$stem_linewidth, alpha = p$stem_alpha,
        inherit.aes = FALSE, show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = function(model) layout(model)$mutations,
        ggplot2::aes(x = lollipop_x, y = lollipop_y,
          colour = amino_acid_group), size = p$point_size, alpha = 0.9,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_colour_manual("Alternative amino acid", values = group_colours,
                                   breaks = group_names, drop = FALSE) +
      ggplot2::scale_fill_manual(values = domain_colours, guide = "none") +
      ggplot2::scale_y_continuous(breaks = log10(count_breaks + 1),
                                 labels = scales::label_comma()(count_breaks),
                                 expand = ggplot2::expansion(mult = c(0.025, 0.08))) +
      ggplot2::labs(y = "Allele count (log scale)") +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1,
        override.aes = list(size = 2.4, alpha = 1))) + base_theme
  }
  overview_layout <- function(model) hla_a_lollipop_layout(
    ggexon::query_protein_mutations(model, annotation = "hla_substitutions"),
    model, p$head_half_span, p
  )
  overview <- draw(overview_layout) +
    ggplot2::geom_text(data = region_labels, ggplot2::aes(
                       x = midpoint, label = short_label, colour = I(label_colour)),
                       y = -0.21, fontface = "bold", show.legend = FALSE,
                       size = text_mm * 0.8, family = p$family) +
    ggplot2::scale_x_continuous(breaks = c(1, 50, 100, 150, 200, 250, 300, 341),
                               limits = c(-1, 343), expand = c(0, 0)) +
    ggplot2::labs(
      title = "HLA-A  /  allelic diversity",
      subtitle = sprintf("%s named alleles     /     %s substitutions     /     %s variable residues",
                         format(nrow(evidence$alleles), big.mark = ","),
                         format(nrow(mutations), big.mark = ","),
                         length(unique(mutations$mature_position))),
      x = "Mature-protein residue  /  reference A*02:01:01:01",
      caption = paste0("One head per amino-acid substitution; stems retain true residue anchors. ",
                       "Head positions fan locally to separate alternatives.\n",
                       "IPD-IMGT/HLA 3.66.0 · eligible complete 365-aa precursors · ",
                       "database counts, not population frequencies. CP: connecting peptide; TM: transmembrane.")
    )
  focus <- mutations[mutations$mature_position == evidence$focus_position, , drop = FALSE]
  focus_layout <- function(model) {
    selected <- ggexon::query_protein_mutations(model, annotation = "hla_substitutions")
    selected <- selected[selected$mature_position == evidence$focus_position, , drop = FALSE]
    prepared <- hla_a_lollipop_layout(selected, model, p$focus_half_span, p)
    prepared$mutations$label <- paste0(selected$mutation, "  ·  ",
      format(selected$allele_count, big.mark = ",", trim = TRUE))
    prepared
  }
  focus_plot <- draw(focus_layout) +
    ggplot2::geom_text(data = function(model) focus_layout(model)$mutations, ggplot2::aes(
      x = lollipop_x, y = lollipop_y, label = label
    ), nudge_y = 0.12, size = text_mm, family = p$family, fontface = "bold") +
    ggplot2::scale_x_continuous(breaks = evidence$focus_position) +
    ggplot2::coord_cartesian(xlim = evidence$focus_position + c(-4.8, 4.8),
                             ylim = c(-0.4, max(log10(focus$allele_count + 1)) + 0.45)) +
    ggplot2::labs(title = paste("Residue", evidence$focus_position),
      subtitle = "Each alternative has its own count; every stem shares one residue anchor.",
      x = "True mature-protein residue (heads spread for readability)",
      caption = "Counts describe eligible named alleles in the database, not population frequencies.") +
    ggplot2::theme(legend.position = "none")
  list(overview = overview, focus = focus_plot,
       overview_data = overview_layout(hla), focus_data = focus_layout(hla))
}
