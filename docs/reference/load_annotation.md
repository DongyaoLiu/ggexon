# Load annotations into Syn-aware objects

Imports the annotation file as a `GRanges` object and stores it in the
active feature-annotation slots. The imported ranges are lightly
normalized so downstream query and translation methods can use
consistent metadata columns.

## Usage

``` r
load_annotation(x, annotation = NULL, individual = NULL)

# S4 method for class 'SynFeatureAnnotation'
load_annotation(x, annotation = NULL, individual = NULL)

# S4 method for class 'SynAnnotation'
load_annotation(x, annotation = NULL, individual = NULL)

# S4 method for class 'SynIndividual'
load_annotation(x, annotation = NULL, individual = NULL)

# S4 method for class 'SynSpecies'
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
  "compact_synspecies",
  "caenorhabditis_XZ1516.gff3",
  package = "ggexon"
)

ann <- SynFeatureAnnotation(name = "default", annotation_file = ann_path)
ann <- load_annotation(ann)

ind <- SynIndividual(
  annotation_file = ann_path,
  genome_file = genome_waiver(),
  id = "XZ1516"
)
ind <- load_annotation(ind)

sp <- SynSpecies(name = "worms") |> add_individual(ind)
sp <- load_annotation(sp)
```
