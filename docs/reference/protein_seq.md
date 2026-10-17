# Access translated protein sequences

Returns the translated protein sequences stored on a `SynIndividual` or
`SynFeatureAnnotation` object.

## Usage

``` r
protein_seq(x)

# S4 method for class 'SynIndividual'
protein_seq(x)

# S4 method for class 'SynFeatureAnnotation'
protein_seq(x)
```

## Arguments

- x:

  A `SynIndividual` or `SynFeatureAnnotation` object.

## Value

A
[`Biostrings::AAStringSet`](https://rdrr.io/pkg/Biostrings/man/XStringSet-class.html)
object, or `NULL` when no protein sequences are stored.

## Examples

``` r
x <- SynIndividual(id = "sample")
protein_seq(x)
#> NULL
```
