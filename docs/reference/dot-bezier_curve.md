# Compute cubic Bézier curve points

Evaluates a cubic Bézier curve defined by four control points.

## Usage

``` r
.bezier_curve(x0, y0, x1, y1, x2, y2, x3, y3, n = 50L)
```

## Arguments

- x0, y0:

  Start point.

- x1, y1:

  First control point.

- x2, y2:

  Second control point.

- x3, y3:

  End point.

- n:

  Number of evaluation points.

## Value

A data.frame with columns `x` and `y`.
