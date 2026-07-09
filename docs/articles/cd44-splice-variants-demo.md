# CD44 Splice Variant Alignment Demo

## Overview

This tutorial compares selected human `CD44` and mouse `Cd44`
protein-coding splice variants in one pairwise genomic view. The example
uses two annotation tracks, one middle
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
panel, and genomic DNA alignments produced with LASTZ from the same
windows used for plotting.

The point is not to draw every transcript. `CD44` has many Ensembl
isoforms, so the bundled dataset keeps a small representative subset:
the RefSeq-backed canonical transcript, a short isoform, an intermediate
isoform, and an expanded isoform in each species. Exons are then
classified as:

- `common`: present in every selected isoform for that species.
- `variable`: absent from at least one selected isoform for that
  species.

The data are bundled under `inst/extdata/cd44_pairwise_ensembl116`.
Coordinates remain in their original genome assemblies: human GRCh38
`chr11` and mouse GRCm39 `chr2`. The mouse `Cd44` gene is on the reverse
strand, so the plot below reverses the mouse panel at draw time while
keeping tick labels in original genomic coordinates.

``` r

library(ggexon)

demo_dir <- system.file("extdata", "cd44_pairwise_ensembl116", package = "ggexon")
species <- read.delim(file.path(demo_dir, "cd44_species.tsv"), check.names = FALSE)
isoforms <- read.delim(file.path(demo_dir, "cd44_selected_isoforms.tsv"), check.names = FALSE)

isoform_table <- isoforms[, c(
  "display_name", "transcript_name", "transcript_id", "exon_count",
  "selection_reason"
)]
names(isoform_table) <- c("Species", "Transcript", "Ensembl transcript", "Exons", "Reason")
knitr::kable(isoform_table)
```

| Species | Transcript | Ensembl transcript | Exons | Reason |
|:---|:---|:---|---:|:---|
| Human | CD44-208 | ENST00000428726 | 18 | RefSeq-backed Ensembl canonical transcript (NM_000610) |
| Human | CD44-201 | ENST00000263398 | 9 | RefSeq-backed shorter CD44 splice isoform (NM_001001391) |
| Human | CD44-210 | ENST00000434472 | 10 | RefSeq-backed intermediate CD44 splice isoform (NM_001202555) |
| Human | CD44-242 | ENST00000904013 | 18 | RefSeq-backed expanded CD44 splice isoform (NM_001440324/NM_001440326) |
| Mouse | Cd44-201 | ENSMUST00000005218 | 19 | RefSeq-backed Ensembl canonical transcript (NM_009851) |
| Mouse | Cd44-203 | ENSMUST00000099673 | 9 | RefSeq-backed shorter Cd44 splice isoform (NM_001039151) |
| Mouse | Cd44-204 | ENSMUST00000111190 | 11 | RefSeq-backed intermediate Cd44 splice isoform (NM_001177787) |
| Mouse | Cd44-208 | ENSMUST00000111198 | 16 | RefSeq-backed expanded Cd44 splice isoform (NM_001177785) |

## Pairwise splice-variant plot

The plotting table has one row per exon per selected isoform. The
y-position is assigned by `selection_order`, which keeps the four
selected isoforms visible instead of overplotting them on one baseline.
The fill maps to the exon-level splice role, while the transcript
backbone is drawn in neutral grey with `transcript_backbone_fill`.

``` r

exons <- read.delim(file.path(demo_dir, "cd44_selected_exons.tsv"), check.names = FALSE)
links <- read.delim(file.path(demo_dir, "cd44_nuclinks_lastz.tsv"), check.names = FALSE)

track_levels <- c("human", "link_human_mouse", "mouse")
track_labels <- c(
  human = sprintf("Human CD44 (%s)", species$chr[species$species == "human"]),
  link_human_mouse = "",
  mouse = sprintf("Mouse Cd44 (%s)", species$chr[species$species == "mouse"])
)

exons$track <- factor(exons$track, levels = track_levels)
links$track <- factor(links$track, levels = track_levels)
exons$exon_role <- factor(exons$exon_role, levels = c("common", "variable"))

splice_palette <- c(common = "#246B5A", variable = "#D88A3D")

ggexon() +
  geom_nuclink(
    data = links,
    mapping = aes(
      tspecies = tspecies,
      tchr = tchr,
      tstart = tstart,
      tend = tend,
      qspecies = qspecies,
      qchr = qchr,
      qstart = qstart,
      qend = qend,
      strand = strand,
      group = group
    ),
    fill = "#6A8FB8",
    colour = NA,
    alpha = 0.32,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_exon(
    data = exons,
    mapping = aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      transcripts = transcripts,
      strand = strand,
      track = track,
      type = type,
      fill = exon_role
    ),
    exon_height = 0.5,
    transcript_backbone_ratio = 0.08,
    transcript_backbone_fill = "grey82",
    transcript_backbone_colour = NA,
    colour = "grey25",
    linewidth = 0.12
  ) +
  facet_genomics(
    vars(track),
    scales = "free_x",
    ncol = 1,
    link_panel_height = 0.32,
    link_axis = "none",
    link_strip = "blank",
    annotation_axis = "bottom",
    reverse_x = "mouse",
    reverse_x_match_by = "track",
    strip.position = "left",
    labeller = ggplot2::as_labeller(track_labels)
  ) +
  scale_fill_manual(values = splice_palette, drop = FALSE, name = "Selected exons") +
  scale_x_continuous(
    labels = function(x) paste0(round(x / 1000), " kb"),
    expand = ggplot2::expansion(mult = c(0.01, 0.01))
  ) +
  labs(x = "Genomic coordinate (mouse panel reversed)", y = NULL) +
  theme_ggexon_track(
    base_size = 8,
    show_x_axis = TRUE,
    show_x_grid = TRUE,
    show_legend = TRUE
  ) +
  theme(
    panel.spacing.y = grid::unit(0.05, "lines"),
    legend.position = "bottom",
    legend.key.height = grid::unit(3, "mm"),
    legend.key.width = grid::unit(6, "mm"),
    plot.margin = margin(6, 8, 6, 6)
  ) +
  theme_ggexon_side_strips("left", base_size = 7.5)
```

![A ggexon pairwise CD44 splice variant plot with four human isoform
rows above a nucleotide-link panel and four mouse isoform rows below it.
The mouse x axis is reversed. Common exons are green, variable exons are
orange, transcript backbones are grey, and blue ribbons connect LASTZ
genomic alignment
blocks.](cd44-splice-variants-demo_files/figure-html/cd44-plot-1.png)

Selected human CD44 and mouse Cd44 splice variants with the mouse panel
reversed. Exons present in every selected isoform are green; variable
splice exons are orange. LASTZ genomic DNA alignment blocks are drawn as
nucleotide links in the middle panel.

## What the alignment layer contributes

[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
uses a table of target/query genomic intervals. Here those intervals
come from LASTZ alignments of genomic DNA windows spanning the human and
mouse gene loci. The link rows are deliberately kept separate from the
exon annotation rows:

- annotation rows describe selected transcript structures and exon
  membership;
- link rows describe nucleotide-level genomic alignment blocks;
- [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
  creates the middle panel and injects the source-panel coordinates that
  [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
  needs.

``` r

homology_candidates <- read.delim(
  file.path(demo_dir, "cd44_exon_homology_ranked.tsv"),
  check.names = FALSE
)

summary_table <- data.frame(
  Metric = c(
    "Selected human isoforms",
    "Selected mouse isoforms",
    "LASTZ link blocks retained",
    "Ranked exon-pair candidates",
    "Reciprocal-best exon-pair candidates"
  ),
  Value = c(
    length(unique(isoforms$transcript_id[isoforms$species == "human"])),
    length(unique(isoforms$transcript_id[isoforms$species == "mouse"])),
    nrow(links),
    nrow(homology_candidates),
    sum(homology_candidates$reciprocal_best)
  )
)
knitr::kable(summary_table)
```

| Metric                               | Value |
|:-------------------------------------|------:|
| Selected human isoforms              |     4 |
| Selected mouse isoforms              |     4 |
| LASTZ link blocks retained           |    18 |
| Ranked exon-pair candidates          |    12 |
| Reciprocal-best exon-pair candidates |     3 |

## Reusing the pattern

For splice-variant comparisons, keep the biological feature colors on
the exon rectangles and use neutral styling for schematic structure. In
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md),
that means setting `transcript_backbone_fill` and
`transcript_backbone_colour`. The same idea is available for
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
through its intron and terminal-arrow controls, and for
[`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md)
through `tag_arrow_fill` and `tag_arrow_colour`.

The reproducible data-preparation script is stored in
`data-raw/cd44_pairwise_ensembl116/build-cd44-demo.R`. It records the
Ensembl release, source URLs, UCSC sequence windows, LASTZ command,
selected isoforms, common-exon calls, and exon-homology candidate
tables.
