# ggexon themes for genomic tracks

`theme_ggexon_track()` provides compact defaults for genomic tracks. It
keeps the x axis visible, hides the y axis used only for track geometry,
and removes visual noise from minor grids and legends.

## Usage

``` r
theme_ggexon_track(
  base_size = 8,
  base_family = "",
  show_x_axis = TRUE,
  show_y_axis = FALSE,
  show_x_grid = TRUE,
  show_legend = FALSE
)

theme_ggexon_genomictree(
  base_size = 8,
  base_family = "",
  show_x_axis = TRUE,
  show_y_axis = FALSE,
  show_x_grid = TRUE,
  show_legend = FALSE
)
```

## Arguments

- base_size:

  Base font size passed to
  [`ggplot2::theme_minimal()`](https://rdrr.io/pkg/ggplot2/man/ggtheme.html).

- base_family:

  Base font family passed to
  [`ggplot2::theme_minimal()`](https://rdrr.io/pkg/ggplot2/man/ggtheme.html).

- show_x_axis:

  Logical; show x-axis labels, ticks, and axis line.

- show_y_axis:

  Logical; show y-axis labels and ticks. Defaults to `FALSE` because
  most ggexon geoms use y as a track-position coordinate.

- show_x_grid:

  Logical; show major x grid lines.

- show_legend:

  Logical; keep the legend. Defaults to `FALSE`.

## Value

A ggplot2 theme object.

## Details

`theme_ggexon_genomictree()` builds on `theme_ggexon_track()` for
stacked tree-aligned genomic panels. The tree-tip labels use
`strip.text.y`, and the custom tree branch-length axis reuses the x-axis
text styling.
