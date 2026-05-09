# Build link line grobs from already-transformed data

Internal helper shared by `GeomGeneLink` and other geoms (e.g.
`GeomGeneLabel`). Takes coordinate-transformed data and returns a grid
grob (or gList).

## Usage

``` r
.draw_link_grobs_raw(data, link_type)
```

## Arguments

- data:

  A data.frame with columns `x`, `y`, `xend`, `yend`, `colour`,
  `linewidth`, `linetype`, `alpha`. Coordinates must be in native units.

- link_type:

  One of `"straight"`, `"elbow"`, or `"spline"`.

## Value

A grob.
