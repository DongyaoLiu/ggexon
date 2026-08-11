# Filter a pairwise alignment by minimum PAF alignment length

Filter a pairwise alignment by minimum PAF alignment length

## Usage

``` r
filter_pairwise_alignment(x, filter = 200, alignment = NULL)

# S4 method for class 'SynPairAlignment'
filter_pairwise_alignment(x, filter = 200, alignment = NULL)

# S4 method for class 'SynSpecies'
filter_pairwise_alignment(x, filter = 200, alignment = NULL)
```

## Arguments

- x:

  A `SynSpecies` or `SynPairAlignment` object.

- filter:

  Minimum `alen` to keep.

- alignment:

  Optional alignment name when `x` is a `SynSpecies`.

## Value

An updated `SynPairAlignment` or `SynSpecies` object.

## Details

This is an S4 generic that dispatches on the class of `x`.

## Examples

``` r
paf_path <- system.file(
  "extdata", "cd44_pairwise_ensembl116", "cd44_lastz.paf",
  package = "ggexon"
)
pair <- SynPairAlignment(
  name = "mouse_vs_human",
  query_individual = "mouse",
  target_individual = "human",
  file = paf_path
)
pair <- filter_pairwise_alignment(pair, filter = 200)
```
