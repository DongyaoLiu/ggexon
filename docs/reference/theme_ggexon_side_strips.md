# Place ggexon facet strips on the side

A theme helper for putting facet strip labels (e.g. species tracks) on
the left or right of the panels instead of stacked on top, which
reclaims the vertical row a top strip would otherwise occupy. This
styles the side-strip text so labels read horizontally and sit just
outside the panels.

## Usage

``` r
theme_ggexon_side_strips(
  side = c("right", "left"),
  base_size = 8,
  face = "bold",
  background = "grey96"
)
```

## Arguments

- side:

  `"right"` or `"left"`. Must match the `strip.position` passed to
  [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md).

- base_size:

  Base font size for the strip text.

- face:

  Font face for the strip text (e.g. `"bold"`).

- background:

  Strip-background fill colour, or `NA`/`"none"` for none.

## Value

A ggplot2 theme object to add to a ggexon plot.

## Details

The actual strip *position* is set by the facet, so pair this with
`facet_genomics(strip.position = "<side>")` using the same `side`.

## See also

[`theme_ggexon_track()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_track.md),
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)

## Examples

``` r
p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(ggplot2::vars(cyl), ncol = 1, strip.position = "left")
p + theme_ggexon_side_strips("left")
```
