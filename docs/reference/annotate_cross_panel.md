# Cross-panel annotation specification

Create a lightweight specification for annotations that will be resolved
after plot build and injected during gtable rendering.

## Usage

``` r
annotate_cross_panel(
  data,
  from,
  to,
  ...,
  colour = NULL,
  color = NULL,
  linewidth = NULL,
  linetype = NULL,
  alpha = NULL,
  geom = "line"
)
```

## Arguments

- data:

  A data frame containing anchor information.

- from, to:

  Aesthetic mappings describing the source and target anchors. Both
  mappings must include `panel`, `x`, and `y`.

- ..., colour, color, linewidth, linetype, alpha:

  Optional styling parameters stored on the specification.

- geom:

  Annotation geometry. V1 only supports `"line"`.

## Value

An object of class `"cross_panel_annotation"`.
