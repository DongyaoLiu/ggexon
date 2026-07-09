# Draw gene tags as exon bodies with strand-direction triangles

`geom_genetag()` draws each stranded gene as a rectangular exon-like
body plus a symmetric terminal triangle. The triangle apex points toward
the strand-specific end of the feature. It is designed for gene-level
summaries, including ggtree side panels generated with
[`ggtree::facet_plot()`](https://rdrr.io/pkg/ggtree/man/facet-plot.html).

## Usage

``` r
geom_genetag(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  exon_height = NULL,
  height = NULL,
  arrow_width = NULL,
  arrow_fraction = 0.18,
  tag_arrow_fill = NULL,
  tag_arrow_colour = NULL,
  gene_layout = "single",
  gene_lane_gap = 0.15,
  species = NULL,
  chr = NULL,
  subset = NULL,
  feature_type = "gene",
  show_label = TRUE,
  label_position = NULL,
  label_direction = NULL,
  label_offset_fraction = NULL,
  label_link = NULL,
  label_link_type = NULL,
  collapse_tandem = NULL,
  check_overlap = FALSE,
  label_max_lanes = NULL,
  label_panel_width = NULL,
  label_genes = NULL,
  label_filter = NULL,
  label_match_by = NULL,
  label_match = NULL,
  label_size = NULL,
  label_colour = NULL,
  label_alpha = NULL,
  label_family = NULL,
  label_fontface = NULL,
  label_lineheight = NULL,
  label_link_colour = NULL,
  label_link_linewidth = NULL,
  label_link_linetype = NULL,
  label_link_alpha = NULL,
  panel_width_mm = NULL,
  panel_width_inch = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments. `inherit.aes` defaults to `FALSE` so
  the layer can be used cleanly in
  [`ggtree::facet_plot()`](https://rdrr.io/pkg/ggtree/man/facet-plot.html)
  side panels.

- exon_height:

  Total tag height in y-axis units. Defaults to `0.8`.

- height:

  Deprecated-compatible alias for `exon_height`.

- arrow_width:

  Optional width of the terminal triangle in x-axis units. When `NULL`,
  width is calculated from `arrow_fraction`.

- arrow_fraction:

  Fraction of each gene span used for the terminal triangle when
  `arrow_width` is `NULL`.

- tag_arrow_fill, tag_arrow_colour:

  Optional fixed fill and outline for the terminal strand-direction
  triangle. When `NULL`, the triangle uses the same inherited aesthetics
  as the gene tag body.

- gene_layout:

  Gene-body overlap layout. `"single"` keeps all gene tags on one
  baseline. `"stack"` assigns overlapping gene spans to separate
  vertical lanes. `"nested"` also assigns lanes, ordering containing
  spans before contained spans so embedded genes are visible inside
  broad gene-level annotations.

- gene_lane_gap:

  Gap between stacked gene-body lanes, as a fraction of `exon_height`.

- species:

  Optional species / individual identifier when `data` is a
  `SynSpecies`.

- chr:

  Optional chromosome / seqname restriction when `data` is Syn-backed.

- subset:

  Optional numeric length-2 genomic window to keep.

- feature_type:

  Feature type passed to
  [`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md).
  Defaults to `"gene"`.

- show_label:

  Logical; draw gene labels. Defaults to `TRUE`.

- label_position:

  Label placement mode. `"auto"` draws labels inside tags when they fit
  and falls back outside otherwise; `"inside"` keeps the previous
  inside-only behavior; `"outside"` draws all labels outside the tag;
  `"none"` suppresses labels.

- label_direction:

  Outside label position. Accepts `"top"`, `"bottom"`, `"center"`, or
  colon-delimited combinations such as `"top:bottom"`. Outside fallback
  treats `"center"` labels that do not fit as `"top"`.

- label_offset_fraction:

  Distance between the tag and outside label line, as a fraction of
  `exon_height`.

- label_link:

  Logical; draw leader links for outside labels.

- label_link_type:

  Leader line style: `"straight"`, `"elbow"`, or `"spline"`.

- collapse_tandem:

  When `TRUE`, consecutive outside labels with the same displayed
  `label` in a track are collapsed into one label.

- check_overlap:

  Logical passed to text drawing for opt-in label overlap suppression.

- label_max_lanes:

  Maximum number of vertical lanes available for outside labels on each
  side of a track. Defaults to `3`.

- label_panel_width:

  Panel width used for label layout. The default `"auto"` measures the
  final panel viewport at draw time. A positive numeric value is
  interpreted as millimetres.

- label_genes:

  Optional gene selector for partial labeling. A character vector
  applies globally; a named list applies per track.

- label_filter:

  Semantic label filter. Multiple values are OR-combined. `"all"`
  preserves the default behavior; `"homology_hit"` labels both
  query-side hits and matching visible reference genes;
  `"homology_query_hit"` and `"homology_reference_hit"` label only one
  side; `"species_specific"` labels non-homologous non-reference genes;
  `"homology_anchor"`, `"homology_visible"`, and `"homology_offtrack"`
  require strip-scale metadata.

- label_match_by:

  Columns used to match `label_genes`. `"auto"` checks common gene and
  reference-gene identifier columns.

- label_match:

  Matching mode for `label_genes`: `"exact"` or `"regex"`.

- label_size, label_colour, label_alpha, label_family, label_fontface,
  label_lineheight:

  Fixed label styling used when `show_label = TRUE`. These can also be
  mapped as aesthetics with names such as `aes(label_colour = ...)`.

- label_link_colour, label_link_linewidth, label_link_linetype,
  label_link_alpha:

  Fixed leader-link styling for outside labels. These can also be mapped
  as aesthetics with the same names.

- panel_width_mm, panel_width_inch:

  Optional panel width for estimating whether labels fit inside
  transformed gene tags.

## Value

A ggplot layer.
