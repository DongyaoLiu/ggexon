# Constructor for SynLayout

Constructor for SynLayout

## Usage

``` r
SynLayout(
  panels,
  layout_type = "custom",
  free = list(x = FALSE, y = FALSE),
  exon_height = NA_real_,
  y_scale = NA_real_,
  x_translation = NA_real_,
  metadata = list()
)
```

## Arguments

- panels:

  Panel layout table. At minimum it must contain `PANEL`, `ROW`, `COL`,
  and `track`.

- layout_type:

  Layout strategy label, such as `"custom"` or `"chain"`.

- free:

  List with logical `x` and `y` entries controlling free-scale behavior
  across panels.

- exon_height:

  Default shared exon/gene/gene-label height resolved by syn-aware
  annotation geoms.

- y_scale:

  Default shared y-axis scaling for layout-aware geoms.

- x_translation:

  Default shared x-axis translation applied to layout-aware geoms.

- metadata:

  Optional metadata list.

## Value

A `SynLayout` object.
