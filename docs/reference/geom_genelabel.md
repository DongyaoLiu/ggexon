# Draw gene labels on exon tracks

`geom_genelabel()` places one text label per transcript or gene span on
an exon-style genomic track. The `label_direction` parameter accepts
colon-delimited combinations to distribute labels across positions using
modulo assignment.

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
  label_direction = NULL,
  label_offset_fraction = NULL,
  link_type = NULL,
  collapse_tandem = NULL,
  show_link = NULL,
  species = NULL,
  chr = NULL,
  subset = NULL,
  panel_width_mm = NULL,
  panel_width_inch = NULL,
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

- label_direction:

  One or more label positions joined with `:`, e.g. `"top"`, `"bottom"`,
  `"top:bottom"`, or `"bottom:top:center"`. Each gene receives a
  position based on its track index modulo the number of position
  tokens:

  - `"top"` — all labels above the highest track.

  - `"bottom"` — all labels below the lowest track.

  - `"top:bottom"` — odd-indexed genes above, even-indexed below.

  - `"bottom:top:center"` — gene 1 bottom, gene 2 top, gene 3 centre
    (and repeats). Genes assigned `"center"` have their label placed on
    the gene body. If the label text is wider than the gene span, the
    label falls back to `"top"`.

  Valid tokens: `"top"`, `"bottom"`, `"center"`. Default `"top"`.

- label_offset_fraction:

  Distance between the exon tracks and the label line, expressed as a
  fraction of `exon_height`. Default `0.3`.

- link_type:

  Leader line style: `"straight"` (direct line), `"elbow"` (right-angle
  bend via vertical then horizontal segment), or `"spline"` (smooth
  Bézier curve). Default `"straight"`. Centre-fitting labels do not draw
  leader lines.

- collapse_tandem:

  When `TRUE`, consecutive genes with identical labels (tandem
  duplications) share a single label connected to all gene bodies by a
  bracket-style connector. Default `FALSE`.

- show_link:

  When `TRUE` (the default), leader lines are drawn between gene bodies
  and labels. Set to `FALSE` to suppress all leader lines (only the text
  labels are rendered).

- species:

  Optional species / individual identifier when `data` is a
  `SynSpecies`.

- chr:

  Optional chromosome / seqname restriction when `data` is Syn-backed.

- subset:

  Optional numeric length-2 genomic window to keep.

- panel_width_mm:

  Estimated width of the genomic panel in millimetres. Used to convert
  text size into data-coordinate units for label placement and collision
  avoidance. Default `300` (≈ A4/US-letter panel width). Increase this
  for wide output (e.g. `ggsave(width = 40)`).

- panel_width_inch:

  Same as `panel_width_mm` but in inches. When both are provided,
  `panel_width_inch` takes precedence. One inch = 25.4 mm.

## Value

A ggplot2 layer using the internal `GeomGeneLabel` ggproto.
