# Draw fixed-size gene boxes with internal strand arrows

`geom_genebox()` represents each gene with a square of fixed physical
size and a horizontal arrow indicating transcription direction. Because
the box dimensions are measured in millimetres, the symbol stays square
under free genomic scales and coordinate transformations.

## Usage

``` r
geom_genebox(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  box_size = 3,
  anchor = c("middle", "start", "end"),
  arrow_colour = NULL,
  arrow_linewidth = 0.45,
  arrow_head_size = 0.7,
  species = NULL,
  chr = NULL,
  subset = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments. When column names are literally `x`,
  `y`, and `strand`, they are mapped automatically.

- box_size:

  Side length of each square in millimetres. Defaults to `3`.

- anchor:

  Genomic point derived for Syn-backed data: the annotated
  initiation-codon centre or 5-prime terminal-CDS proxy (`"start"`),
  midpoint between the selected start and end anchors (`"middle"`), or
  annotated stop-codon centre or 3-prime terminal-CDS proxy (`"end"`).
  Defaults to `"middle"`.

- arrow_colour:

  Optional fixed colour for the internal arrow. `NULL` chooses black or
  white separately for each box to contrast with its fill.

- arrow_linewidth:

  Width of the internal arrow in millimetres.

- arrow_head_size:

  Length of the closed triangular arrow head in millimetres.

- species:

  Optional individual identifier for `SynSpecies` input.

- chr:

  Optional chromosome or seqname restriction for Syn-backed data.

- subset:

  Optional numeric length-two genomic window for Syn-backed data.

## Value

A ggplot layer.

## Details

For ordinary data frames, map `x`, `y`, and `strand`. In a ggexon plot
backed by a
[SynIndividual](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
or
[SynSpecies](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
object, the layer selects, for each gene, the protein-coding transcript
with the greatest genomic span. `anchor = "start"` uses the middle
nucleotide of a complete annotated initiation codon, `anchor = "end"`
uses the middle nucleotide of a complete annotated stop codon, and
`anchor = "middle"` uses the genomic midpoint between those two anchors.
Associated `start_codon` or `stop_codon` records are complete only when
their total feature width is exactly three nucleotides and every piece
matches the selected transcript's seqname, strand, and bounds.
Otherwise, the middle position of the corresponding terminal CDS triplet
is used as a positional proxy; this fallback does not verify an ATG or
stop-codon sequence.

Unknown strands (`NA`, `"*"`, or values other than `"+"` and `"-"`)
retain their square but omit the internal arrow. Reversing the x scale
or x coordinate direction reverses the visible arrow direction as well.
Supplying a `slot` column allows
[`strip_scale_x()`](https://dongyaoliu.github.io/ggexon/reference/strip_scale_x.md)
with `slot_order` to align curated comparison slots exactly. Slot
membership is supplied metadata, not an inference of one-to-one homology
or evolutionary loss. When a track has enough ordered genes to infer
whether the synthetic template reverses its raw genomic direction, the
internal arrows are corrected automatically;
[`strip_scale_x()`](https://dongyaoliu.github.io/ggexon/reference/strip_scale_x.md)
warns and keeps the raw direction when this is underdetermined.

Syn-derived data retain `genomic_x`, `anchor_start`, `anchor_middle`,
`anchor_end`, `anchor_mode`, `transcript_id`, transcript-span columns,
coding bounds, gene identifiers, and (for `SynSpecies`) injected
homology metadata. The per-end `initiation_anchor_source` and
`stop_anchor_source` columns record either an explicit codon feature or
`"terminal_CDS_positional_proxy"`; `initiation_anchor_fallback`,
`stop_anchor_fallback`, and `any_anchor_fallback` flag proxy use. These
columns make transcript selection, anchor interpretation, and downstream
alignment auditable.
