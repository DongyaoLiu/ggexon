# Plot a tree-aligned genomic track layout

`plot_ggtree_genomic_alignment()` assembles one tree panel beside one
species-local genomic panel per tree tip. The tree panel spans all
genomic rows, and all panels share the tree-derived tip y positions.

## Usage

``` r
plot_ggtree_genomic_alignment(
  alignment,
  mapping = ggplot2::aes(fill = gene),
  tree_width = grid::unit(1.5, "in"),
  label_width = grid::unit(0.75, "in"),
  track_width = grid::unit(1, "null"),
  panel_height = grid::unit(0.5, "in"),
  exon_height = 0.6,
  arrow_width = NULL,
  arrow_fraction = 0.18,
  tree_colour = "black",
  tree_linewidth = 0.5,
  show_track_labels = TRUE,
  show_x_axis = TRUE,
  base_size = 9,
  label_gp = grid::gpar(fontsize = 9),
  ...
)
```

## Arguments

- alignment:

  A `ggtree_genomic_alignment` object returned by
  [`compile_ggtree_genomic_alignment()`](https://dongyaoliu.github.io/ggexon/reference/compile_ggtree_genomic_alignment.md).

- mapping:

  Aesthetic mapping passed to
  [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md).
  Defaults to `ggplot2::aes(fill = gene)`.

- tree_width, track_width, label_width:

  Grid units controlling column widths.

- panel_height:

  Grid unit controlling each tip row height.

- exon_height, arrow_width, arrow_fraction:

  Passed to
  [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md).

- tree_colour, tree_linewidth:

  Tree segment styling.

- show_track_labels:

  Logical; draw tip/individual labels between the tree and genomic
  panels.

- show_x_axis:

  Logical; draw x axes for the tree and genomic panels. Defaults to
  `TRUE` so species-local coordinate systems are visible.

- base_size:

  Base font size used when `show_x_axis = TRUE`.

- label_gp:

  Optional [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) for track
  labels.

- ...:

  Additional fixed aesthetics passed to
  [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md).

## Value

A `gtable` object. Print it or call
[`grid::grid.draw()`](https://rdrr.io/r/grid/grid.draw.html).
