# Draw link lines between pairs of coordinates

`geom_genelink()` draws link lines between start points `(x, y)` and end
points `(xend, yend)`. Three line styles are supported via the
`link_type` parameter:

## Usage

``` r
geom_genelink(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  link_type = "straight",
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://rdrr.io/pkg/ggplot2/man/aes.html).
  Required: `x`, `y`, `xend`, `yend`.

- data:

  The data to be displayed.

- stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- link_type:

  Line style: `"straight"`, `"elbow"`, or `"spline"`. Default
  `"straight"`.

## Value

A ggplot2 layer using the internal `GeomGeneLink` ggproto.

## Details

- `"straight"` (default) — a single straight segment.

- `"elbow"` — two right-angle segments with a bend at `(x, yend)`.

- `"spline"` — a smooth cubic Bézier curve from `(x, y)` to
  `(xend, yend)` with auto-derived control points.

This geom is designed to work generically with any coordinate data,
making it reusable for gene labels, mutation annotations, or other
link-line needs.
