# Subset a feature annotation layer by genomic window

Returns a windowed snapshot of a
[`SynFeatureAnnotation`](https://dongyaoliu.github.io/ggexon/reference/SynFeatureAnnotation.md).
The current active annotation and its base annotation are trimmed to the
requested genomic interval. Patch history and derived caches are cleared
so the returned object behaves as a self-contained view of the selected
region.

## Usage

``` r
subset_feature_annotation(
  x,
  annotation = NULL,
  individual = NULL,
  chr = NULL,
  start = NULL,
  end = NULL,
  coords = NULL,
  gene = NULL,
  transcript = NULL
)
```

## Arguments

- x:

  A `SynFeatureAnnotation`, `SynIndividual`, or `SynSpecies` object.

- annotation:

  Optional feature-annotation layer name when `x` is a `SynIndividual`
  or `SynSpecies`. Defaults to the active feature annotation.

- individual:

  Optional individual name when `x` is a `SynSpecies`.

- chr:

  Chromosome or sequence name to keep.

- start, end:

  Optional numeric window bounds. If one bound is omitted, the helper
  expands to the start or end of the selected sequence present in the
  annotation.

- coords:

  Optional coordinate string in the form `"V_RagTag:21559983-21620009"`.
  Use either `coords` or `chr`/`start`/`end`.

## Value

Returns an object of the same top-level class supplied to `x`: a
`SynFeatureAnnotation`, updated `SynIndividual`, or updated
`SynSpecies`.

## Details

This is an S4 generic that dispatches on the class of `x`.

## Examples

``` r
ann_path <- system.file(
  "extdata",
  "caenorhabditis_XZ1516.gff3",
  package = "ggexon"
)
ind <- SynIndividual(
  annotation_file = ann_path,
  genome_file = genome_waiver(),
  id = "XZ1516"
) |>
  load_annotation()

gr <- annotation_data(ind)
chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
start <- IRanges::start(gr)[[1L]]
end <- IRanges::end(gr)[[1L]]

ann_view <- subset_feature_annotation(
  get_annotation(ind, "default"),
  chr = chr,
  start = start,
  end = end
)
#> subset_feature_annotation() kept 8 rows, 1 genes, and 1 transcripts.
ind_view <- subset_feature_annotation(ind, chr = chr, start = start, end = end)
#> subset_feature_annotation() kept 8 rows, 1 genes, and 1 transcripts.
```
