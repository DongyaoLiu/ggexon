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
  species = NULL,
  chr = NULL,
  subset = NULL,
  feature_type = "gene",
  show_label = TRUE,
  label_size = 3,
  label_colour = "black",
  label_family = "sans",
  label_fontface = 1,
  label_lineheight = 1.2,
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

  Logical; draw gene labels inside tags. Defaults to `TRUE`.

- label_size, label_colour, label_family, label_fontface,
  label_lineheight:

  Fixed label styling used when `show_label = TRUE`.

- panel_width_mm, panel_width_inch:

  Optional panel width for estimating whether labels fit inside
  transformed gene tags.

## Value

A ggplot layer.
