# X-only strip scale for gene tracks

`strip_scale_x()` normalizes gene-tag or gene-box x coordinates. Its
default layout gives genes and intergenic gaps predictable visual
widths. Homology mode can compress species-specific local runs and
translate tracks to align the most conserved block against an explicit
reference track. Exact-template mode maps gene anchors to a complete
synthetic `slot_order`, independently of the raw genomic distances
between genes.

## Usage

``` r
strip_scale_x(
  gene_gap_ratio = NULL,
  align = c("left", "right", "center"),
  reference_track = NULL,
  homo_align = FALSE,
  gene_order = c("genomic", "reference"),
  species_specific_ratio = 0.5,
  secondary_homology_ratio = 0.75,
  species_ratio = NULL,
  collapse_contiguous_slot = TRUE,
  block_align = c("conserved", "left", "center", "right", "none"),
  guide = c("range", "none"),
  slot_order = NULL
)

strip_scale(...)
```

## Arguments

- gene_gap_ratio:

  Ratio of full gene visual width to intergenic gap visual width. When
  `NULL`, the ratio is estimated from the densest track. It is not used
  when `slot_order` is supplied.

- align:

  Alignment for level-1, non-homology tracks with fewer genes than the
  widest track.

- reference_track:

  Optional single reference track name for homology-aware layout. This
  is the preferred alias for `homo_align`.

- homo_align:

  `FALSE` for level-1 layout only, or a single character reference track
  name for homology-aware layout. `TRUE` is not supported. Prefer
  `reference_track` for new code.

- gene_order:

  Gene ordering strategy. `"genomic"` keeps each track in its native
  genomic order. `"reference"` orders query tracks by the resolved
  homolog order in `reference_track`, keeping unmapped local runs
  between the nearest surrounding reference-ordered homologs. When
  `slot_order` is supplied, that exact order governs the layout
  regardless of this setting.

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
  genomic bp range guide using the panel window when one is available,
  and otherwise the visible gene range; `"none"` suppresses the custom
  guide.

- slot_order:

  Optional character vector defining exact shared comparison slots from
  left to right. Gene rows are matched through `slot`, falling back to
  `reference_gene` and then `gene_key`. The selected genomic anchor of
  every matching row is mapped to the center of its slot, so unoccupied
  template positions remain visible. Slot membership is supplied
  metadata, not an inference of one-to-one homology or evolutionary
  loss. This synthetic-template mode does not require, and cannot be
  combined with, `reference_track`.

- ...:

  Arguments passed from the compatibility wrapper `strip_scale()` to
  `strip_scale_x()`.

## Value

A ggexon strip-scale-x specification, added to a plot with `+`.

## Details

Once genomic x distances are stripped, gene-body overlap lanes are
collapsed to a single baseline per gene-tag layer. Outside labels remain
coordinated independently, so label lanes can still alternate above and
below the shared gene-body line.

In exact-template mode, visible gene-box direction is inferred
separately for each panel and track from the rank correlation between
genomic anchors and template-slot positions. A track needs at least two
distinct genomic anchors in at least two distinct slots and a non-zero
rank correlation. Otherwise `strip_scale_x()` warns once per build and
uses `+1` (no template-driven direction reversal) for every
underdetermined track.
