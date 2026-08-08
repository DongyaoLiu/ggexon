# Build a grid SynLayout from a locus set

Build a grid SynLayout from a locus set

## Usage

``` r
locus_grid_layout(
  x,
  locus_set = NULL,
  row_order = NULL,
  col_order = NULL,
  free = list(x = TRUE, y = FALSE)
)
```

## Arguments

- x:

  A `SynSpecies` or `SynLocusSet` object.

- locus_set:

  Optional attached locus-set name or `SynLocusSet` object when `x` is a
  `SynSpecies`.

- row_order:

  Optional row order for `row_group`.

- col_order:

  Optional column order for `col_group`.

- free:

  Free-scale settings for the returned `SynLayout`.

## Value

A `SynLayout` object.
