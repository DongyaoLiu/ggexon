# Plot interval-native BigWig coverage

`geom_coverage()` draws raw coverage values as rectangles spanning each
stored BigWig interval. With a `SynIndividual` or `SynSpecies` data
source, the layer resolves an attached
[SynBigWigAnnotation](https://dongyaoliu.github.io/ggexon/reference/SynBigWigAnnotation-class.md)
and queries the effective panel window. A plain data frame can instead
provide `xmin`, `xmax`, `coverage`, and `track` aesthetics directly.

## Usage

``` r
geom_coverage(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  annotation = NULL,
  species = NULL,
  bigwig = NULL,
  ref_chr = NULL,
  subset = NULL,
  annotation_type = NULL,
  y_threshold = NULL,
  x_threshold = NULL,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- annotation:

  Optional name of an attached `SynBigWigAnnotation`. When omitted, each
  individual must have exactly one attached BigWig annotation.

- species:

  Optional individual selector when plotting a `SynSpecies`.

- bigwig, ref_chr, subset, annotation_type, y_threshold, x_threshold:

  Deprecated file-driven coverage arguments. Attach a
  `SynBigWigAnnotation` and use effective panel windows instead.

## Value

A ggplot2 layer.

## Details

Coverage scores are plotted unchanged. The layer does not normalize,
smooth, threshold, or expand signal records to one row per base.
Inclusive genomic interval endpoints are retained in `interval_start`,
`interval_end`, `genomic_xmin`, and `genomic_xmax` in the built layer
data; rectangle edges are shifted by half a base so adjacent intervals
meet without overlap or gaps.
