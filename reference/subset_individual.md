# Subset a SynIndividual by genomic window

Returns a new
[`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
whose feature annotation layers are trimmed to the requested genomic
window. By default all attached
[`SynFeatureAnnotation`](https://dongyaoliu.github.io/ggexon/reference/SynFeatureAnnotation.md)
layers are subsetted, while non-feature annotation layers remain
attached unchanged. Object-level derived caches are cleared so the
result is ready to use as a clean windowed view.

## Usage

``` r
subset_individual(
  x,
  individual = NULL,
  chr = NULL,
  start = NULL,
  end = NULL,
  coords = NULL,
  annotations = c("all_feature", "active")
)
```

## Arguments

- x:

  A `SynIndividual` or `SynSpecies` object.

- individual:

  Optional individual name when `x` is a `SynSpecies`.

- chr:

  Chromosome or sequence name to keep.

- start, end:

  Optional numeric window bounds. If one bound is omitted, the helper
  expands to the start or end of the selected sequence present in the
  active feature annotation.

- coords:

  Optional coordinate string in the form `"V_RagTag:21559983-21620009"`.
  Use either `coords` or `chr`/`start`/`end`.

- annotations:

  One of `"all_feature"` or `"active"`. Controls whether all feature
  annotation layers are trimmed, or only the active one.

## Value

A `SynIndividual` object.

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
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'load_annotation': `annotation_file` must not contain missing or empty paths.

gr <- annotation_data(ind)
#> Error in annotation_data(ind): could not find function "annotation_data"
chr <- as.character(GenomeInfoDb::seqnames(gr))[[1L]]
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'seqnames': object 'gr' not found
start <- IRanges::start(gr)[[1L]]
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'start': object 'gr' not found
end <- IRanges::end(gr)[[1L]]
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'end': object 'gr' not found

ind_window <- subset_individual(ind, chr = chr, start = start, end = end)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'subset_individual': object 'ind' not found
```
