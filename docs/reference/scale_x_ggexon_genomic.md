# Compress introns while keeping genomic x-axis labels

`scale_x_ggexon_genomic()` adds a panel-specific x transform for
exon-style genomic tracks. Exonic intervals keep their original length,
while the gaps between exon-union intervals are compressed by
`intron_factor`. Axis breaks are drawn at compressed display positions
but labelled with the original genomic coordinates.

## Usage

``` r
scale_x_ggexon_genomic(
  intron_factor = 10,
  species = NULL,
  match_by = c("auto", "species", "strain", "id", "track"),
  breaks = waiver(),
  labels = waiver(),
  minor_breaks = NULL,
  n.breaks = 5,
  guide = waiver()
)
```

## Arguments

- intron_factor:

  Numeric compression factor for intronic gaps. For example, `10` draws
  a 10 kb intron as 1 kb while leaving exon widths unchanged.

- species:

  Optional character vector selecting the species, strains, ids, or
  tracks that should receive intron compression. When `NULL`, all
  eligible panels are compressed. Unselected panels stay on their
  original genomic coordinate scale.

- match_by:

  Panel-layout column used to match `species`. `"auto"` checks common
  layout columns such as `species`, `strain`, `id`, and `track`.

- breaks:

  Genomic-coordinate breaks. Use
  [`waiver()`](https://dongyaoliu.github.io/ggexon/reference/waiver.md)
  for pretty breaks over the original genomic range, `NULL` to hide
  breaks, a numeric vector, or a function that receives original genomic
  limits.

- labels:

  Break labels. Use
  [`waiver()`](https://dongyaoliu.github.io/ggexon/reference/waiver.md)
  for
  [`scales::label_number()`](https://rdrr.io/pkg/scales/man/label_number.html),
  `NULL` for no labels, a character vector, or a function applied to
  original genomic break values.

- minor_breaks:

  Minor breaks in original genomic coordinates. Defaults to `NULL`.

- n.breaks:

  Approximate number of pretty major breaks when `breaks = waiver()`.

- guide:

  Axis guide. Use
  [`waiver()`](https://dongyaoliu.github.io/ggexon/reference/waiver.md)
  or `"genomic"` for ordinary genomic-coordinate ticks, `"none"` to hide
  transformed x-axis ticks, or
  [`guide_x_ggexon_piecewise()`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md)
  for representative exon/intron scale bars.

## Value

A ggexon x-scale specification.

## Details

This scale is intended for exon-structure layers such as
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md)
and
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md).
It builds one transform per panel from the union of exon-like intervals,
so multiple transcripts in the same panel stay aligned.

Use
[`guide_x_ggexon_piecewise()`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md)
in `guide` when the axis should display representative exon and intron
scale bars instead of ordinary genomic ticks.
