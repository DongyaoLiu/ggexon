# Return the individual identifiers stored on a `SynSpecies`

Returns the names used to index the `SynIndividual` objects attached to
a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md).
These usually match `names(individuals(x))`. If unnamed entries are
present, the accessor falls back to `syn_id()` for those objects.

## Usage

``` r
individual_names(x)
```

## Arguments

- x:

  A `SynSpecies` object.

## Value

A character vector of individual identifiers in stored order.
