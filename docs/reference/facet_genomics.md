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
  axis.labels = "all"
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
