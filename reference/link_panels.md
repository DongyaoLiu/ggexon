# Link shared ids across panels

Build a cross-panel annotation spec from one row per anchor. The
resulting specification is resolved after plot build and drawn during
rendering.

## Usage

``` r
link_panels(
  data,
  id,
  panel,
  x,
  y,
  ...,
  colour = NULL,
  color = NULL,
  linewidth = NULL,
  linetype = NULL,
  alpha = NULL
)
```

## Arguments

- data:

  A data frame containing one row per anchor.

- id, panel, x, y:

  Column names identifying the anchor id, panel, and position.

- ..., colour, color, linewidth, linetype, alpha:

  Optional styling parameters stored on the specification.

## Value

An object of class `"cross_panel_annotation"`.
