# Annotate pairwise DNA differences with anchored sequence detail

`geom_alignseq()` draws data prepared by
[`alignseq_data()`](https://dongyaoliu.github.io/ggexon/reference/alignseq_data.md)
on annotation panels. In marker mode, all nonmatching events in the
selected regions are drawn; contiguous indels have one marker, labelled
with their length. Only explicitly selected `details` windows receive
DNA callouts on their respective annotation panels. Nearby windows are
merged by the preparation helper.

## Usage

``` r
geom_alignseq(
  data,
  mapping = NULL,
  mode = c("markers", "rows"),
  size = 2.5,
  family = "mono",
  background = TRUE,
  max_columns = 60,
  na.rm = FALSE,
  show.legend = FALSE,
  ...
)
```

## Arguments

- data:

  A table returned by
  [`alignseq_data()`](https://dongyaoliu.github.io/ggexon/reference/alignseq_data.md).
  Must be supplied explicitly. Optional `boundary_after` (logical) and
  `boundary_label` (character) columns mark verified boundaries, such as
  splice donors, after selected bases in the equally spaced sequence
  detail. These do not alter the DNA strings.

- mapping:

  Optional aesthetic overrides. Prepared metadata and x/y/track are
  mapped automatically. Use anchor `track` names matching the annotation
  facet, never a link-panel track.

- mode:

  `"markers"` (default) or `"rows"`. Rows display all selected columns
  as an equally spaced DNA row in each sequence's own annotation panel,
  connected to its genomic extent. Row mode requires one contiguous
  alignment-column selection per sequence.

- size:

  DNA character size in mm.

- family:

  Character font, default `"mono"`.

- background:

  Logical; draw pale nucleotide backgrounds.

- max_columns:

  Maximum columns in a callout or row view (default 60). Longer windows
  raise an error requesting a narrower selection.

- na.rm, show.legend, ...:

  Standard layer arguments. Event colours and shapes are fixed:
  substitution blue circle, insertion orange triangle, deletion purple
  inverted triangle, ambiguous grey cross. No fill scale is added, so
  existing exon and link palettes remain available.

## Value

A ggplot2 layer using a native annotation geom. Cartesian coordinates
are required. In ggexon, use with annotation data and
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md).

## Details

Smooth curved connectors retain their exact genomic and callout
endpoints. Marker and connector anchors follow
[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md)
automatically. Callout columns are equally spaced in panel space
**after** genomic scaling: they are schematic alignment detail, not an
additional genomic ruler. Each panel displays only its own sequence in
the supplied orientation. Markers describe the query relative to the
reference, not evolutionary change.

## See also

[`alignseq_data()`](https://dongyaoliu.github.io/ggexon/reference/alignseq_data.md),
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md),
[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md)
