# Project mature-protein substitutions through the exact reference allele CDS.
load_hla_a_gene_model <- function(hla, data_dir = system.file(
  "extdata", "hla_a_lollipop", package = "ggexon"
)) {
  structure <- utils::read.delim(file.path(data_dir, "hla_a_gene_structure.tsv"),
                                 stringsAsFactors = FALSE)
  individual <- ggexon::SynIndividual(
    id = "HLA_A0201", annotation_file = file.path(data_dir, "hla_a_reference.gff3"),
    metadata = list(reference_allele = hla$reference,
                    coordinate_system = "HLA00005 allele-local genomic bases, one-based closed")
  )
  individual <- ggexon::load_annotation(individual)
  mutations <- hla$mutations
  mutations$gene_id <- "HLA_A0201"
  mutations$mature_position <- mutations$position
  mutations$position <- mutations$precursor_position
  mutations$sample_count <- mutations$allele_count
  individual <- ggexon::add_annotation(individual,
    ggexon::SynProteinMutationAnnotation(
      name = "hla_substitutions", mutation_file = "<computed HLA-A allele summary>",
      mutation_data = mutations, keytype = "gene_id", lazy = FALSE
    ), set_active = FALSE)
  projection <- ggexon::project_mutations_to_genome(
    individual, annotation = "hla_substitutions", transcripts = "HLA_A0201_tx"
  )
  # A splice-split codon yields several exonic fragments. Anchor its head on
  # the longest fragment, never the midpoint of an intron-spanning envelope.
  fragments <- split(seq_len(nrow(projection)), projection$mutation)
  picks <- vapply(fragments, function(indices) {
    indices[which.max(projection$xmax[indices] - projection$xmin[indices] + 1)]
  }, integer(1))
  anchors <- projection[picks, , drop = FALSE]
  anchors <- anchors[match(hla$mutations$mutation, anchors$mutation), , drop = FALSE]
  anchors$anchor <- (anchors$xmin + anchors$xmax) / 2
  rownames(anchors) <- NULL
  list(individual = individual, projection = projection, anchors = anchors,
       exons = structure[structure$type == "exon", ],
       introns = structure[structure$type == "intron", ], structure = structure)
}

plot_hla_a_gene_example <- function(hla, gene, intron_factor = 3,
                                    parameters = hla_a_plot_parameters) {
  p <- parameters
  exons <- gene$exons
  # Draw base edges, not inclusive base centres: widths now equal bp counts.
  exons$xmin <- exons$start - 0.5
  exons$xmax <- exons$end + 0.5
  exons$ymin <- -0.34
  exons$transcripts <- "HLA_A0201_tx"
  exons$strand <- "+"
  exons$track <- "HLA_A0201"
  exons$group <- 1L
  domains <- data.frame(start = exons$start, end = exons$end, domain = "CDS")
  prepared <- hla_a_lollipop_layout(
    gene$anchors, hla, half_span = p$head_half_span * 3, parameters = p,
    position = "anchor", domains = domains, track_length = max(exons$end)
  )
  group_names <- c("Aliphatic", "Aromatic", "Polar", "Basic", "Acidic", "Gly / Pro")
  count_breaks <- c(1, 10, 100, 1000, 5000)
  template <- plot_hla_a_example(hla, p)$overview
  plot <- ggexon::ggexon(exons, ggplot2::aes(
    xmin = xmin, xmax = xmax, ymin = ymin, transcripts = transcripts,
    strand = strand, track = track, type = type, group = group
  )) +
    ggexon::geom_exon2(
      annotation_type = "exon", compress_introns = FALSE,
      exon_height = 0.26, intron_peak = 0.12, intron_shape = "chevron",
      fill = "#263D52", colour = "#263D52", linewidth = 0.35,
      transcript_backbone_colour = "#647E8A", show.legend = FALSE
    ) +
    ggplot2::geom_line(data = prepared$stems, ggplot2::aes(
      x = lollipop_x, y = lollipop_y, group = lollipop_curve_id,
      colour = amino_acid_group
    ), linewidth = p$stem_linewidth, alpha = p$stem_alpha,
    inherit.aes = FALSE, show.legend = FALSE) +
    ggplot2::geom_point(data = prepared$mutations, ggplot2::aes(
      x = lollipop_x, y = lollipop_y, colour = amino_acid_group
    ), size = p$point_size, alpha = 0.9, inherit.aes = FALSE) +
    ggplot2::geom_text(data = exons, ggplot2::aes(
      x = (start + end) / 2, label = number
    ), y = -0.53, size = p$text_size_pt / ggplot2::.pt * 0.8,
    colour = "#344854", inherit.aes = FALSE) +
    ggexon::scale_x_ggexon_genomic(
      intron_factor = intron_factor,
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
  list(plot = plot, prepared = prepared)
}
