# Draw gene labels on exon tracks

`geom_genelabel()` places one text label per transcript or gene span on
an exon-style genomic track. The `label_direction` parameter accepts
colon-delimited combinations to distribute labels across multiple
positions (top, bottom, centre) using modulo assignment. Labels placed
at `"top"` or `"bottom"` connect to their gene body with leader lines;
centre-positioned labels sit directly on the gene body. Overlapping
labels are pushed apart horizontally.

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

  Leader line style: `"straight"` (direct line) or `"elbow"`
  (right-angle bend via vertical then horizontal segment). Default
  `"straight"`. Centre-fitting labels do not draw leader lines.

- collapse_tandem:

  When `TRUE`, consecutive genes with identical labels (tandem
  duplications) share a single label connected to all gene bodies by a
  bracket-style connector. Default `FALSE`.

- species:

  Optional species / individual identifier when `data` is a
  `SynSpecies`.

- chr:

  Optional chromosome / seqname restriction when `data` is Syn-backed.

- subset:

  Optional numeric length-2 genomic window to keep.

## Value

A ggplot2 layer using the internal `GeomGeneLabel` ggproto.
