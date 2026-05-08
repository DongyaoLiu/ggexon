# Convert an ODGI multiple alignment into a pairwise alignment

Builds a PAF-like pairwise link table for one selected pair of
individuals from an ODGI node table, raw `.og` graph, or
[`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md)
with `format = "odgi"`. The returned object is a
[`SynPairAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynPairAlignment.md)
with `format = "odgi"`, so it can be added directly to a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
object and consumed by
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md).

## Usage

``` r
odgi_pairwise_alignment(
  x,
  query_individual,
  target_individual,
  name = NULL,
  individuals = NULL,
  odgi = NULL,
  python = NULL,
  file = NULL,
  metadata = list()
)
```

## Arguments

- x:

  A
  [`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md)
  with `format = "odgi"`, an ODGI node-table `data.frame`, a path to an
  ODGI node-table TSV, or a raw `.og` graph path.

- query_individual:

  Query-side individual identifier.

- target_individual:

  Target-side individual identifier.

- name:

  Optional pairwise alignment label. Defaults to
  `"<alignment>__<query>__<target>"`.

- individuals:

  Optional individual mapping used when `x` is not already a
  `SynMultiAlignment`. If named, the names must match ODGI path labels.

- odgi:

  Optional path to the `odgi` executable. Used when `x` is an `.og`
  graph file.

- python:

  Optional path to the Python interpreter. Used when `x` is an `.og`
  graph file.

- file:

  Optional source file to store on the returned object.

- metadata:

  Optional metadata list.

## Value

A
[`SynPairAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynPairAlignment.md)
object with `format = "odgi"` and a cached PAF-like pairwise table in
its `data` slot.
