# Draw exon-style genomic features

`geom_exon()` draws exon-like genomic intervals as filled rectangles
with a transcript backbone and direction indicator. It supports ordinary
ggplot2 aesthetic mappings and can resolve `SynSpecies` /
`SynIndividual` inputs lazily during plot build.

## Usage

``` r
geom_exon(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  transcripts_track_ratio = NULL,
  exon_height = NULL,
  transcript_backbone_ratio = NULL,
  x_translation = NULL,
  subset = NULL,
  annotation_type = "exon",
  species = NULL,
  chr = NULL,
  breakdata = NULL,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  In addition to the required positional aesthetics, Syn-backed exon
  layers expose canonical identifier columns `transcript_id`, `gene_id`,
  and `gene_name` for use in aesthetic mappings.

- data:

  A data frame, `SynSpecies`, or `SynIndividual` object.

- stat, position:

  Standard ggplot2 layer arguments.

- ...:

  Additional parameters passed on to the layer, including fixed
  aesthetics such as `fill`, `colour`, and `alpha`.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are removed silently.

- show.legend:

  Logical. Should this layer be included in the legend?

- transcripts_track_ratio:

  Optional transcript track ratio used by the ggexon layout helpers.

- exon_height:

  Optional exon rectangle height.

- transcript_backbone_ratio:

  Relative backbone height as a fraction of `exon_height`. Defaults to
  `0.1`.

- x_translation:

  Optional x offset applied before drawing.

- subset:

  Optional numeric length-2 genomic window to keep. When omitted for
  Syn-backed data, the full annotation range is used.

- annotation_type:

  Feature type to keep, defaults to `"exon"`.

- species:

  Optional species / individual identifier when `data` is a
  `SynSpecies`. When omitted, ggexon resolves exon data for all stored
  individuals.

- chr:

  Optional chromosome / seqname restriction when `data` is Syn-backed.

- breakdata:

  Optional break specification passed to `addbreak()`.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics rather than combining
  with them.

## Value

A ggplot2 layer using
[`GeomExon`](https://dongyaoliu.github.io/ggexon/reference/GeomExon.md).

## Details

For Syn-backed layers, ggexon adds canonical identifier columns to the
resolved exon table so users can map aesthetics with expressions such as
`aes(fill = gene_id)`, `aes(fill = gene_name)`, or
`aes(fill = transcript_id)`.

When `data` is a `SynSpecies`, omitting `species` uses all stored
individuals. Omitting `subset` keeps the full annotation table, while
supplying `chr` without `subset` limits the layer to that seqname.
