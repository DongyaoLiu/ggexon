# Store a locus-grid layout on a SynSpecies

Store a locus-grid layout on a SynSpecies

## Usage

``` r
use_locus_grid(
  x,
  locus_set = NULL,
  row_order = NULL,
  col_order = NULL,
  free = list(x = TRUE, y = FALSE)
)
```

## Arguments

- x:

  A `SynSpecies` object.

- locus_set:

  Optional attached locus-set name or `SynLocusSet` object.

- row_order:

  Optional row order.

- col_order:

  Optional column order.

- free:

  Free-scale settings passed to
  [`locus_grid_layout()`](https://dongyaoliu.github.io/ggexon/reference/locus_grid_layout.md).

## Value

The updated `SynSpecies`.
