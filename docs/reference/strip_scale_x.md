# X-only strip scale for gene-tag tracks

`strip_scale_x()` normalizes gene-tag x coordinates so genes and
intergenic gaps occupy predictable visual widths. In homology mode, it
can compress species-specific local runs and translate tracks to align
the most conserved block against an explicit reference track.

## Usage

``` r
strip_scale_x(
  gene_gap_ratio = NULL,
  align = c("left", "right", "center"),
  homo_align = FALSE,
  species_specific_ratio = 0.5,
  secondary_homology_ratio = 0.75,
  species_ratio = NULL,
  collapse_contiguous_slot = TRUE,
  block_align = c("conserved", "left", "center", "right", "none"),
  guide = c("range", "none")
)

strip_scale(...)
```

## Arguments

- gene_gap_ratio:

  Ratio of full gene visual width to intergenic gap visual width. When
  `NULL`, the ratio is estimated from the densest track.

- align:

  Alignment for level-1, non-homology tracks with fewer genes than the
  widest track.

- homo_align:

  `FALSE` for level-1 layout only, or a single character reference track
  name for homology-aware layout. `TRUE` is not supported.

- species_specific_ratio:

  Visual width of a species-specific gene or collapsed run relative to a
  homologous gene.

- secondary_homology_ratio:

  Visual width of a homologous off-track or duplicate gene relative to a
  primary visible homologous gene.

- species_ratio:

  Deprecated alias for `species_specific_ratio`.

- collapse_contiguous_slot:

  Logical; when `TRUE`, contiguous species-specific genes are compressed
  into one local run slot.

- block_align:

  Homology-mode track translation. `"conserved"` aligns the
  highest-support reference homology block by median center offset,
  without requiring query genes to appear in the same order. `"left"`,
  `"center"`, and `"right"` align each local track span to the reference
  span. `"none"` leaves level-2 local coordinates untranslated.

- guide:

  Strip-scale x-axis guide. `"range"` draws a simple per-track begin/end
  genomic bp range guide; `"none"` suppresses the custom guide.

- ...:

  Arguments passed from the compatibility wrapper `strip_scale()` to
  `strip_scale_x()`.

## Value

A ggexon strip-scale-x specification, added to a plot with `+`.
