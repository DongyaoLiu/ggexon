# Draw WormWeb-style exon-intron schematics

`geom_exon2()` draws a publication-style transcript schematic inspired
by WormWeb exon/intron cartoons. Compared with
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md),
it draws introns explicitly between neighbouring feature blocks, can
compress long introns, and uses thinner boxes for UTR features when
UTR/CDS rows are available.

## Usage

``` r
geom_exon2(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  transcripts_track_ratio = NULL,
  exon_height = NULL,
  x_translation = NULL,
  subset = NULL,
  annotation_type = "exon",
  species = NULL,
  chr = NULL,
  breakdata = NULL,
  compress_introns = TRUE,
  intron_width = NULL,
  intron_shape = c("chevron", "flat"),
  chevron_direction = c("up", "down"),
  utr_height = 0.45,
  cds_height = 1,
  intron_peak = 0.35,
  arrow_width = NULL,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

- data:

  A data frame, `SynSpecies`, or `SynIndividual` object.

- stat, position:

  Standard ggplot2 layer arguments.

- ...:

  Additional parameters passed on to the layer.

- na.rm:

  If `FALSE`, missing values are removed with a warning.

- show.legend:

  Logical. Should this layer be included in the legend?

- transcripts_track_ratio:

  Optional transcript track ratio used by the ggexon layout helpers.

- exon_height:

  Optional maximum exon rectangle height.

- x_translation:

  Optional x offset applied before drawing.

- subset:

  Optional numeric length-2 genomic window to keep.

- annotation_type:

  Feature type to keep. The default `"exon"` matches
  [`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md).
  Use `"all"` to keep exon, CDS, and UTR-like rows so UTR/CDS heights
  can differ.

- species:

  Optional species / individual identifier when `data` is a
  `SynSpecies`.

- chr:

  Optional chromosome / seqname restriction when `data` is Syn-backed.

- breakdata:

  Optional break specification passed to `addbreak()`.

- compress_introns:

  Logical. If `TRUE`, gaps between neighbouring feature blocks are
  replaced by schematic intron gaps.

- intron_width:

  Width used for compressed introns. When `NULL`, ggexon derives a width
  from the median feature width.

- intron_shape:

  `"chevron"` for angled WormWeb-like connectors or `"flat"` for
  straight intron lines.

- chevron_direction:

  Direction for chevron introns, either `"up"` or `"down"`.

- utr_height:

  Relative height for UTR-like rows.

- cds_height:

  Relative height for CDS rows. Exon rows also use this height when no
  explicit UTR/CDS distinction is present.

- intron_peak:

  Relative height of the chevron peak.

- arrow_width:

  Width of the terminal strand-direction triangle. When `NULL`, ggexon
  derives a width from the terminal exon block.

- inherit.aes:

  If `FALSE`, overrides inherited aesthetics.

## Value

A ggplot2 layer using `GeomExon2`.

## Details

The layer keeps the same Syn-aware grammar as
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md):
it can consume a data frame directly, or lazily resolve `SynIndividual`
/ `SynSpecies` inputs during plot build.
