# Facet genomic panels by tree-tip order

`facet_genomictree()` lays out one genomic panel per tree-matched
individual, ordered by the y positions in
[`geom_genomic_tree()`](https://dongyaoliu.github.io/ggexon/reference/geom_genomic_tree.md).
During rendering, ggexon injects a single tree panel that spans all
genomic rows.

## Usage

``` r
facet_genomictree(
  facets = ggplot2::vars(track),
  scales = "free_x",
  shrink = TRUE,
  labeller = "label_value",
  as.table = TRUE,
  switch = deprecated(),
  drop = TRUE,
  strip.position = "top",
  axes = "margins",
  axis.labels = "all",
  show_tree_x_axis = TRUE,
  label_position = c("left", "right", "none"),
  label_width = grid::unit(0.7, "in"),
  track_width = NULL
)
```

## Arguments

- facets:

  Faceting variables. Defaults to `ggplot2::vars(track)`.

- scales:

  One of `"fixed"`, `"free_x"`, `"free_y"`, or `"free"`.

- shrink:

  Passed through to the facet.

- labeller:

  A labeller specification.

- as.table:

  Logical; whether panels are laid out like a table.

- switch:

  Deprecated ggplot2 argument.

- drop:

  Logical; drop unused facet levels?

- strip.position:

  Position of facet strips.

- axes:

  Which axes to draw.

- axis.labels:

  Which axis labels to draw.

- show_tree_x_axis:

  Logical; draw the tree branch-length x axis.

- label_position:

  Position for tree-tip labels. One of `"left"`, `"right"`, or `"none"`.
  `"left"` places labels between the tree and genomic panels.

- label_width:

  Grid unit width for the tip-label column.

- track_width:

  Grid unit width for the genomic track panel column. Defaults to `NULL`
  (use the facet's default width, typically `unit(1, "null")` filling
  remaining space). Set to a fixed unit (e.g. `unit(30, "in")`) or a
  `"null"` unit to control the ratio between tree, labels, and tracks.

## Value

A `FacetGenomicTree` ggproto object.
