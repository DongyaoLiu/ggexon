# Render a built ggexon plot to a gtable

Render a built ggexon plot to a gtable

## Usage

``` r
ggexon_gtable(data)
```

## Arguments

- data:

  A built `ggexon` plot object.

## Value

A [gtable::gtable](https://gtable.r-lib.org/reference/gtable.html)
object ready to draw with
[`grid::grid.draw()`](https://rdrr.io/r/grid/grid.draw.html).
