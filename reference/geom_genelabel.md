# Draw gene labels on exon tracks

`geom_genelabel()` places one text label per transcript or gene span on
an exon-style genomic track. It uses the same Syn-backed lazy data
resolution as
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md),
so labels can be drawn from `SynIndividual` or `SynSpecies` containers
as well as from precomputed data frames.

## Usage

``` r
geom_genelabel(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  x_translation = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA,
  exon_height = NULL,
  y_scale = NULL,
  species = NULL,
  chr = NULL,
  subset = NULL,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- x_translation:

  Optional x offset applied before drawing.

- exon_height:

  Optional exon rectangle height used when preparing track coordinates.

- y_scale:

  Optional y scaling factor for the track layout.

- species:

  Optional species / individual identifier when `data` is a
  `SynSpecies`.

- chr:

  Optional chromosome / seqname restriction when `data` is Syn-backed.

- subset:

  Optional numeric length-2 genomic window to keep.

## Value

A ggplot2 layer using the internal `GeomGeneLabel` ggproto.
