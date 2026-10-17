# Access stored projected protein-domain tables

Returns the named list of projected protein-domain tables stored on a
`SynIndividual` object.

## Usage

``` r
projected_domains(x)

# S4 method for class 'SynIndividual'
projected_domains(x)
```

## Arguments

- x:

  A `SynIndividual` object.

## Value

A named list of stored projected protein-domain tables.

## Examples

``` r
x <- SynIndividual(id = "sample")
projected_domains(x)
#> list()
```
