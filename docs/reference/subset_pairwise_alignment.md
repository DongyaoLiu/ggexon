# Subset a pairwise alignment by query/target regions

Subset a pairwise alignment by query/target regions

## Usage

``` r
subset_pairwise_alignment(x, subset, alignment = NULL)

# S4 method for class 'SynPairAlignment'
subset_pairwise_alignment(x, subset, alignment = NULL)

# S4 method for class 'SynSpecies'
subset_pairwise_alignment(x, subset, alignment = NULL)
```

## Arguments

- x:

  A `SynSpecies` or `SynPairAlignment` object.

- subset:

  Named character vector/list with one region per species for one or
  both alignment partners, e.g.
  `c(XZ1516 = "RagTag_V:21550000-21680000", N2 = "V:20450000-20451000")`,
  `c(XZ1516 = "RagTag_V:21550000-21680000")`, or
  `c(XZ1516 = "RagTag_V")`.

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
pair <- subset_pairwise_alignment(pair, subset = c(mouse = "mouse"))
```
