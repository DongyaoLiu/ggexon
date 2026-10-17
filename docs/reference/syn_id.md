# Access a SynIndividual identifier

Returns the identifier assigned to a `SynIndividual` object.

## Usage

``` r
syn_id(x)

# S4 method for class 'SynIndividual'
syn_id(x)
```

## Arguments

- x:

  A `SynIndividual` object.

## Value

A length-one character identifier.

## Examples

``` r
x <- SynIndividual(id = "sample")
syn_id(x)
#> [1] "sample"
```
