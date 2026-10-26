# Draw splice junction arcs on annotation or coverage panels

`geom_junction()` connects genomic donor and acceptor anchors with an
upward arc. Map read counts to `label` and `linewidth` to show splice
evidence. Counts describe junction support, not full transcript
abundance.

## Usage

``` r
geom_junction(
  mapping = NULL,
  data = NULL,
  ...,
  height = 0.6,
  label_size = 2.5,
  label_family = "",
  label_vjust = -0.3,
  panel = c("annotation", "coverage"),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments. Required aesthetics are `x`, `xend`,
  and `y`. Optional aesthetics include `yend`, `track`, `height`,
  `label`, `hjust`, `linewidth`, `colour`, `linetype`, and `alpha`. Use
  splice-boundary anchors in the annotation coordinate system. Labels
  default to centered horizontal justification (`hjust = 0.5`); map or
  set `hjust` to reposition crowded labels.

- height:

  Positive, finite rise above the line between the endpoints at their
  displayed x midpoint, in the selected panel's y units, default 0.6.
  The curve's maximum may lie away from the midpoint when `yend` differs
  from `y`. Alternatively map `height` to give junctions different
  rises. An explicitly supplied constant overrides the mapping. The
  layer reserves vertical room for the arc and an additional quarter of
  its rise when a label is present.

- label_size:

  Label text size in mm, default 2.5.

- label_family:

  Label font family.

- label_vjust:

  Vertical label justification relative to the curve at the displayed x
  midpoint, default -0.3. Labels inherit the arc colour and alpha.

- panel:

  Target panel, either `"annotation"` (the default) or `"coverage"`. Use
  `"coverage"` to align arcs with a BigWig track. Map `track` to the
  corresponding coverage track and express `y`, `yend`, and `height` in
  its signal units. Junctions reserve vertical room under ordinary scale
  training, but do not affect signal-derived automatic coverage limits.
  Explicit or automatic
  [`scale_panel_coverage()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_coverage.md)
  limits remain authoritative; choose suitable arc positions or explicit
  limits to retain headroom.

## Value

A ggplot2 layer.

## Details

With
[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md),
endpoints consume the exon-trained map and arcs are generated
afterwards. Labels sit halfway between the displayed endpoints, on the
curve. The layer does not train exon boundaries. In Syn-backed
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
plots, `panel` selects the annotation or coverage panel matching each
junction's `track`. It also works in ordinary ggplot2 plots, where
`panel` does not create a separate panel. Map `yend` to anchor the end
independently; it defaults to `y`. The baseline interpolates linearly
between the endpoint heights in displayed x coordinates, and the curve
rises above it for either endpoint order.

## See also

[`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md),
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md),
[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md)

## Examples

``` r
junctions <- data.frame(donor = c(100, 100), acceptor = c(300, 700),
  y = c(1, 2), reads = c(45, 21))
ggplot2::ggplot(junctions, ggplot2::aes(x = donor, xend = acceptor, y = y)) +
  geom_junction(ggplot2::aes(label = reads, linewidth = reads))
```
