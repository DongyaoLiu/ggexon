# Draw representative exon and intron scale bars for genomic x scaling

`guide_x_ggexon_piecewise()` is used with
[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md)
to replace ordinary x-axis ticks with representative first-exon and
first-intron scale bars. This is useful because intron-compressed tracks
use different display scales for exon and intron regions.

## Usage

``` r
guide_x_ggexon_piecewise(
  by = c("transcripts", "track", "panel"),
  representative = c("first"),
  position = c("bottom"),
  label = TRUE,
  show_exon = TRUE,
  show_intron = TRUE
)
```

## Arguments

- by:

  Grouping used to choose representative intervals. `"transcripts"`
  draws one first-exon/first-intron pair per transcript, `"track"` draws
  one per track, and `"panel"` draws one per panel.

- representative:

  Representative interval selection. Currently only `"first"` is
  supported.

- position:

  Axis position. Currently only `"bottom"` is supported.

- label:

  Logical; draw text labels for representative intervals.

- show_exon, show_intron:

  Logical; include representative exon and/or intron scale bars.

## Value

A ggexon genomic x-axis guide specification.
