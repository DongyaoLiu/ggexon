# Load annotations into Syn-aware objects

Imports the annotation file as a `GRanges` object and stores it in the
active feature-annotation slots. The imported ranges are lightly
normalized so downstream query and translation methods can use
consistent metadata columns.

## Usage

``` r
load_annotation(x, annotation = NULL, individual = NULL)
```

## Arguments

- x:

  A `SynIndividual`, `SynFeatureAnnotation`, or `SynSpecies` object.

- annotation:

  Optional annotation-layer name when `x` is a `SynIndividual` or
  `SynSpecies`. Defaults to the active feature annotation for the
  selected individual.

- individual:

  Optional individual name when `x` is a `SynSpecies`. When omitted, all
  stored individuals are updated.

## Value

An updated object of the same class as `x`.

## Details

When `x` is a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md),
the helper loads the active feature annotation for every stored
[`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
and returns an updated `SynSpecies` object. Pairwise alignments,
multiple alignments, metadata, and layout are left unchanged.

This is an S4 generic that dispatches on the class of `x`.

## Examples

``` r
ann_path <- system.file(
  "extdata",
  "caenorhabditis_XZ1516.gff3",
  package = "ggexon"
)

ann <- SynFeatureAnnotation(name = "default", annotation_file = ann_path)
#> Error in validObject(.Object): invalid class “SynFeatureAnnotation” object: `source_file` must be a non-empty character vector with no empty entries.
ann <- load_annotation(ann)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'load_annotation': object 'ann' not found

ind <- SynIndividual(
  annotation_file = ann_path,
  genome_file = genome_waiver(),
  id = "XZ1516"
)
#> Error: `annotation_file` must not contain missing or empty paths.
ind <- load_annotation(ind)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'load_annotation': object 'ind' not found

sp <- SynSpecies(name = "worms") |> add_individual(ind)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'individual' in selecting a method for function 'add_individual': object 'ind' not found
sp <- load_annotation(sp)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'load_annotation': object 'sp' not found
```
