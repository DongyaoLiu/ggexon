# Uniform gene-width scale for multi-track genomic plots

`strip_scale()` normalizes gene and intergenic region widths across
genomic tracks so that every gene occupies the same visual width and
every intergenic gap occupies the same visual width. The track with the
most genes determines the shared coordinate system; sparser tracks are
aligned within that space.

## Usage

``` r
strip_scale(gene_gap_ratio = NULL, align = c("left", "right", "center"))
```

## Arguments

- gene_gap_ratio:

  Ratio of gene visual width to intergenic gap width. When `NULL` (the
  default), the ratio is derived from the densest track's actual genomic
  proportions (median gene width divided by median gap width).

- align:

  How tracks with fewer genes are positioned within the shared x-axis.
  `"left"` packs genes to the left edge, `"right"` to the right edge,
  `"center"` centres them.

## Value

A ggexon strip-scale specification, added to the plot with `+`.

## Details

This scale is designed for
[`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md)
layers and is mutually exclusive with
[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md).
It works best with `facet_genomictree(scales = "fixed_x")` — the
function will modify the facet to use fixed x scales internally.
