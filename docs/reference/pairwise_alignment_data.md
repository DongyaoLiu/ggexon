# Retrieve pairwise alignment rows

Returns the pairwise alignment table for a
[`SynPairAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynPairAlignment.md)
object or a stored pairwise alignment inside a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
object.

## Usage

``` r
pairwise_alignment_data(x, ...)

# S4 method for class 'SynPairAlignment'
pairwise_alignment_data(x, alignment = NULL, ..., odgi = NULL, python = NULL)

# S4 method for class 'SynSpecies'
pairwise_alignment_data(x, alignment = NULL, ..., odgi = NULL, python = NULL)
```

## Arguments

- x:

  A
  [`SynPairAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynPairAlignment.md)
  or
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  object.

- ...:

  Passed through to the internal alignment-data resolver, including
  options such as `subset` or `filter`.

- alignment:

  Optional alignment name when `x` is a
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md).

- odgi:

  Optional path to the `odgi` executable when ODGI-backed alignments
  need to be loaded.

- python:

  Optional path to the Python interpreter when ODGI-backed alignments
  need helper script execution.

## Value

A `data.frame` containing pairwise alignment rows.
