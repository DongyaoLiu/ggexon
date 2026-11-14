# Inspect the reference structure already attached to the SynIndividual.
hla_a_gene_structure <- function(hla) {
  features <- as.data.frame(ggexon::query_features(hla, feature_type = "exon", all = TRUE))
  features <- features[order(features$start), , drop = FALSE]
  exons <- data.frame(type = "exon", number = as.integer(features$exon_number),
    start = features$start, end = features$end, length = features$width)
  introns <- data.frame(type = "intron", number = seq_len(nrow(exons) - 1L),
    start = head(exons$end, -1L) + 1L, end = tail(exons$start, -1L) - 1L)
  introns$length <- introns$end - introns$start + 1L
  structure <- rbind(exons, introns)
  structure[order(structure$start), , drop = FALSE]
}

# Materialize projection and one shared display layout before plotting.
prepare_hla_a_gene_layout <- function(hla, intron_factor = 3,
                                       parameters = hla_a_plot_parameters) {
  hla <- ggexon::project_mutations_to_genome(
    hla, annotation = "hla_substitutions", transcripts = "HLA_A0201_tx",
    store = TRUE, name = "hla_genomic"
  )
  counts <- ggexon::query_protein_mutations(hla, annotation = "hla_substitutions")$allele_count
  ggexon::prepare_genomic_layout(
    hla, annotation = "hla_genomic", name = "hla_lollipops",
    intron_factor = intron_factor, head_spread = parameters$head_half_span * 3,
    mutation_y_by = "allele_count", mutation_y_trans = "log10",
    mutation_y_range = range(log10(counts + 1)), track_ymin = -0.34, track_ymax = -0.08,
    stem_points = parameters$stem_points
  )
}

hla_a_projected_mutations <- function(hla) {
  list(projection = ggexon::query_genomic_projection(hla, "hla_genomic"),
       anchors = ggexon::query_genomic_projection(hla, "hla_genomic", component = "anchors"))
}

plot_hla_a_gene_example <- function(hla, intron_factor = 3,
                                    parameters = hla_a_plot_parameters) {
  p <- parameters
  exon_labels <- function(model) ggexon::query_genomic_layout(model, "hla_lollipops", "exons")
  group_names <- c("Aliphatic", "Aromatic", "Polar", "Basic", "Acidic", "Gly / Pro")
  count_breaks <- c(1, 10, 100, 1000, 5000)
  template <- plot_hla_a_example(hla, p)$overview
  plot <- ggexon::ggexon(hla) +
    ggexon::geom_exon2(
      genomic_layout = "hla_lollipops",
      intron_peak = 0.12, intron_shape = "chevron",
      fill = "#263D52", colour = "#263D52", linewidth = 0.35,
      transcript_backbone_colour = "#647E8A", show.legend = FALSE
    ) +
    ggexon::geom_protein_lollipop(
      genomic_layout = "hla_lollipops", show_backbone = FALSE, show_domains = FALSE,
      score = "amino_acid_group", stem_colour = NULL,
      stem_linewidth = p$stem_linewidth, stem_alpha = p$stem_alpha,
      point_size = p$point_size, point_alpha = 0.9
    ) +
    ggplot2::geom_text(data = exon_labels, ggplot2::aes(
      x = (xmin + xmax) / 2, label = exon_number
    ), y = -0.53, size = p$text_size_pt / ggplot2::.pt * 0.8,
    colour = "#344854", inherit.aes = FALSE) +
    ggexon::scale_x_ggexon_genomic(
      intron_factor = intron_factor, genomic_layout = "hla_lollipops",
      guide = ggexon::guide_x_ggexon_piecewise(by = "panel")
    ) +
    ggplot2::scale_colour_manual("Alternative amino acid",
      values = stats::setNames(p$wong, group_names), breaks = group_names, drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = log10(count_breaks + 1),
      labels = scales::label_comma()(count_breaks),
      limits = c(-0.8, log10(5001) + 0.18),
      expand = ggplot2::expansion(mult = c(0.035, 0.08))) +
    ggplot2::labs(
      title = "HLA-A  /  variants on the gene model",
      subtitle = sprintf("5,998 named alleles    /    1,212 substitutions    /    8 coding exons + 7 introns    /    introns ÷ %g",
                         intron_factor),
      x = "Reference allele A*02:01:01:01  /  HLA00005 allele-local genomic coordinates",
      y = "Allele count (log scale)",
      caption = paste0("Stem anchors follow the reference CDS, including splice-split codons. ",
                       "The numbered coding-exon model spans ATG to stop.\n",
                       "Exons at native scale; intron gaps ÷ ", intron_factor,
                       ". Scale bars show actual base-pair lengths. ",
                       "Database counts, not population frequencies.")
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1,
      override.aes = list(size = 2.4, alpha = 1))) + template$theme
  list(plot = plot, prepared = list(
    mutations = ggexon::query_genomic_layout(hla, "hla_lollipops", "mutations"),
    stems = ggexon::query_genomic_layout(hla, "hla_lollipops", "stems")
  ))
}
