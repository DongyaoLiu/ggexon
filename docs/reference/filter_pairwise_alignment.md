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
paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")
pair <- SynPairAlignment(
  name = "XZ1516_vs_N2",
  query_individual = "XZ1516",
  target_individual = "N2",
  file = paf_path
)
#> Error in validObject(.Object): invalid class “SynPairAlignment” object: `source_file` must be a non-empty character vector with no empty entries.
pair <- filter_pairwise_alignment(pair, filter = 200)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'filter_pairwise_alignment': object 'pair' not found
```
