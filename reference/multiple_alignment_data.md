# Access data stored on a multiple alignment

Returns the cached parsed representation stored on a
[`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md).
For alignments with `format = "odgi"`, the data can also be loaded
lazily from either a tab-delimited ODGI node-table file or a raw `.og`
graph on disk. When called on a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md),
`alignment` selects which stored multiple alignment to read.

## Usage

``` r
multiple_alignment_data(x, ...)
```

## Arguments

- x:

  A
  [`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md)
  object or a
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  containing one or more multiple alignments.

- ...:

  Reserved for future extensions.

- alignment:

  Optional multiple-alignment name when `x` is a
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md).
  If omitted and exactly one multiple alignment is stored, that
  alignment is used.

- odgi:

  Optional path to the `odgi` executable. Used when an ODGI alignment is
  backed by a raw `.og` graph.

- python:

  Optional path to the Python interpreter. Used when an ODGI alignment
  is backed by a raw `.og` graph.

## Value

A data frame containing the parsed multiple-alignment data.
