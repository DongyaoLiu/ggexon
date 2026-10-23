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
  [`scales::label_number()`](https://scales.r-lib.org/reference/label_number.html),
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

The build first trains one map per reference panel from the union of
original genomic intervals supplied by
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md)
and
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
layers. Multiple displayed transcripts and exon layers contribute to
that union; invisible annotation placeholders are excluded. Other layers
do not train the map, even when they contain gene or transcript
metadata. The trained maps are then applied to genomic x-position
aesthetics across related layers and panels. Points, labels and
whole-gene annotations consume the same map without changing exon/intron
boundaries. Every build trains a fresh registry; subsequent
position-scale training only sets display ranges. Nucleotide links drawn
with
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
are automatically subdivided at source-panel scale boundaries and
transformed with the same maps. Supply their original genomic
coordinates; no manual link-coordinate conversion is needed. Explicit
panel windows are also mapped to compressed display limits. Coverage
drawn with
[`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md)
follows its resolved annotation source panel's map, including when
several coverage tracks share one gene model. Signal intervals are split
at scale boundaries; scores and genomic weighting for automatic coverage
y limits are unchanged. Coverage without an exon-style source stays on
ordinary genomic coordinates. Numeric x-position aesthetics are mapped
centrally. Geoms with specialized geometry still prepare that geometry
separately: nucleotide ribbons subdivide at source boundaries, while DNA
callout lettering is laid out schematically after its genomic anchors
are mapped.

Use
[`guide_x_ggexon_piecewise()`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md)
in `guide` when the axis should display representative exon and intron
scale bars instead of ordinary genomic ticks.
