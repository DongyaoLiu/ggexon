# Facet genomic tracks and link panels

`facet_genomics()` is ggexon's Syn-aware faceting interface. It behaves
like a wrap-style facet for ordinary data, but it can also arrange
comparative genomic panels for `SynSpecies` plots, including annotation
panels and intermediate link panels used by
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md).

## Usage

``` r
facet_genomics(
  facets,
  nrow = NULL,
  ncol = NULL,
  scales = "fixed",
  shrink = TRUE,
  labeller = "label_value",
  as.table = TRUE,
  switch = deprecated(),
  drop = TRUE,
  dir = "h",
  strip.position = "top",
  axes = "margins",
  axis.labels = "all",
  link_panel_height = NULL,
  link_axis = "inherit",
  link_strip = "inherit",
  annotation_axis = "all",
  reverse_x = NULL,
  reverse_x_match_by = c("auto", "species", "strain", "id", "track"),
  xlim = NULL,
  xlim_chr = NULL
)
```

## Arguments

- facets:

  Faceting variables, usually `ggplot2::vars(track)` for Syn layouts.

- nrow, ncol:

  Number of rows and columns in the wrapped layout.

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

- dir:

  Wrapping direction. Single-letter values are normalized using
  `as.table`.

- strip.position:

  Position of facet strips.

- axes:

  Which axes to draw.

- axis.labels:

  Which axis labels to draw.

- link_panel_height:

  Optional relative height for link panels. Supply a single positive
  number to use a `null` unit relative to ordinary panel rows, or a
  single grid unit. When `NULL`, link panels keep the default ggplot2
  facet row height.

- link_axis:

  Link-panel axis handling. `"inherit"` keeps the axes drawn by the
  facet. `"none"` removes both x and y axes from link panels. `"x"`
  keeps only x axes, and `"y"` keeps only y axes.

- link_strip:

  Link-panel strip handling. `"inherit"` keeps link-panel strips.
  `"blank"` removes link-panel strip grobs and collapses horizontal
  strip rows when they contain only link panels.

- annotation_axis:

  Annotation-panel x-axis handling for stacked (single-column) genomic
  layouts. `"all"` (default) keeps the per-panel x-axis that
  `scales = "free_x"` draws on every panel. `"bottom"` keeps the x-axis
  only on the bottom-most annotation panel of each column and blanks the
  interior ones, collapsing the reclaimed axis rows so the panels sit
  compactly. The per-panel free scales are preserved either way.

- reverse_x:

  Annotation panels whose x axis should be drawn in reverse. Use `NULL`
  or `FALSE` for no panel reversal, `TRUE` to reverse all annotation
  panels, or a character vector matched against panel layout columns
  such as `track`, `species`, `strain`, or `id`. Link panels are not
  reversed directly; link x positions inherit the transform from their
  source annotation panels.

- reverse_x_match_by:

  Panel-layout column used to match `reverse_x` character values.
  `"auto"` checks common layout columns such as `species`, `strain`,
  `id`, and `track`.

- xlim:

  Optional panel-specific x limits for annotation panels. Supply a named
  list of numeric length-2 vectors keyed by individual /
  annotation-panel name. If the plot contains only one annotation panel,
  a single numeric length-2 vector is also accepted.

- xlim_chr:

  Optional chromosome / seqname for `xlim`. Supply one character value
  for a single panel, or a named character vector/list keyed by
  individual when `xlim` contains multiple panels. When omitted, ggexon
  tries to infer the seqname from attached alignments or single-seqname
  annotations. Link layers can only be filtered by panel limits when the
  seqname can be resolved.

## Value

A `FacetGenomics` ggproto object.

## Details

For `SynSpecies` inputs, the facet chooses among three layout sources:

- an explicit layout override attached during build

- a stored
  [`SynLayout`](https://dongyaoliu.github.io/ggexon/reference/SynLayout.md)
  on the `SynSpecies` object

- a newly derived chain layout computed from the annotation and link
  layers

When link panels are present, `facet_genomics()` also annotates the
final panel table with source-panel metadata (`t_panel`, `q_panel`) and
vertical link anchors so that
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
can borrow x ranges from the correct annotation panels while drawing
inside the link panel.

## SynSpecies behavior

In Syn-aware builds, `facet_genomics()` is responsible for deciding the
panel structure used by `Layout2`. The returned panel table may include:

- annotation panels for each species track

- link panels inserted between paired species tracks

- `panel_type`, `tspecies`, `qspecies`, `t_panel`, and `q_panel`
  metadata used later by
  [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)

If no Syn-specific layout is available, the facet falls back to ordinary
wrap-style panel generation.

## See also

[SynLayout](https://dongyaoliu.github.io/ggexon/reference/SynLayout.md)
