# Shared background-free theme for ggexon plots

`theme_ggexon_base()` is the common foundation for ggexon's specialized
themes. It uses
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
typography and coordinate grids while removing decorative plot, panel,
strip, border, and legend backgrounds. Strip text, axes, and grid lines
remain available for derived themes to style.

## Usage

``` r
theme_ggexon_base(base_size = 8, base_family = "")
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

[`theme_ggexon_track()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_track.md),
[`theme_ggexon_side_strips()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_side_strips.md),
[`theme_ggexon_pairwise()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_pairwise.md)

## Examples

``` r
ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() +
  theme_ggexon_base()

```
