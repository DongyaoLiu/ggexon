# Resolve one individual from Syn-backed plot input

Normalizes Syn plotting inputs so downstream layer helpers can work with
a single
[`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
object. When `x` is already a `SynIndividual`, the function returns it
unchanged after optionally checking that `species` matches its
identifier. When `x` is a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
collection, the helper selects one stored individual by name.

## Usage

``` r
resolve_syn_individual(x, species = NULL)
```

## Arguments

- x:

  A
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  or
  [`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
  object.

- species:

  Optional individual identifier. When `x` is a `SynSpecies` with more
  than one stored individual, this argument is required.

## Value

A single
[`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
object.

## Details

This function is mainly used inside Syn-aware geoms and query helpers
that allow users to supply either a whole `SynSpecies` object or an
already selected `SynIndividual`.

The helper throws an error when:

- `x` is neither a `SynSpecies` nor a `SynIndividual`

- the supplied `SynSpecies` has no individuals

- `species` is omitted for a `SynSpecies` that stores multiple
  individuals

- `species` does not match any stored individual

- `species` is supplied for a `SynIndividual` but does not match
