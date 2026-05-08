# Set a panel-specific x window on a stored Syn layout

Updates one or more annotation panels in a stored
[`SynLayout`](https://dongyaoliu.github.io/ggexon/reference/SynLayout.md)
or
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
layout. When called on a `SynSpecies` with `individual = NULL` and
`xlim = NULL`, ggexon searches each annotation panel's active feature
layer for subset-window metadata recorded by
[`subset_feature_annotation()`](https://dongyaoliu.github.io/ggexon/reference/subset_feature_annotation.md)
and seeds panel limits from those windows.

## Usage

``` r
set_panel_xlim(x = NULL, individual = NULL, xlim = NULL, xlim_chr = NULL)
```

## Arguments

- x:

  A
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md),
  [`SynLayout`](https://dongyaoliu.github.io/ggexon/reference/SynLayout.md),
  or ggexon plot object.

- individual:

  Optional annotation-panel individual name or names from the layout
  table. Defaults to all annotation panels.

- xlim:

  Optional panel limits. Supply a numeric length-2 vector for one
  individual, or a named list of length-2 numeric vectors keyed by
  individual. When omitted for a `SynSpecies` or ggexon plot backed by a
  `SynSpecies`, ggexon reuses coordinates previously stored by
  [`subset_feature_annotation()`](https://dongyaoliu.github.io/ggexon/reference/subset_feature_annotation.md).

- xlim_chr:

  Optional chromosome / seqname for the panel window. Supply one
  character value for one individual, or a named list/vector keyed by
  individual.

## Value

An updated object of the same class as `x`.
