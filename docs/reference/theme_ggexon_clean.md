# Clean background-free theme for ggexon plots

`theme_ggexon_clean()` keeps the minimal-theme grids, axes, text, and
legend styling while removing panel outlines and facet-strip background
rectangles. It is the named public shortcut for ggexon's background-free
theme contract.

## Usage

``` r
theme_ggexon_clean(base_size = 8, base_family = "")
```

## Arguments

- base_size:

  Base font size passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- base_family:

  Base font family passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

## Value

A ggplot2 theme object.

## See also

[`theme_ggexon_base()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_base.md),
[`theme_ggexon_track()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_track.md),
[`theme_ggexon_side_strips()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_side_strips.md)
