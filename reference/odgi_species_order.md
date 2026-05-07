# Derive a greedy species order from an ODGI multiple alignment

Builds a comparison-chain order for an ODGI multiple alignment by
starting at one reference species and repeatedly choosing the remaining
species that shares the most ODGI nodes with the most recently chosen
species. This is the same heuristic used by
`geom_nuclink(reference = ...)` to arrange ODGI-backed comparison panels
when multiple species are plotted together.

## Usage

``` r
odgi_species_order(
  x,
  reference_species,
  selected_species = NULL,
  alignment = NULL,
  filter_by_len = NULL,
  individuals = NULL,
  odgi = NULL,
  python = NULL
)
```

## Arguments

- x:

  A
  [`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md)
  with `format = "odgi"`, a
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  object containing an ODGI multiple alignment, an ODGI node-table data
  frame, a path to an ODGI node-table TSV, or a raw `.og` graph path.

- reference_species:

  Reference species that seeds the greedy walk.

- selected_species:

  Optional subset of species to include. Species not present in the ODGI
  alignment are ignored.

- alignment:

  Optional alignment name when `x` is a
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  with multiple stored multiple-alignments.

- filter_by_len:

  Optional ODGI node-length filter such as `"> 10"` or `"<= 3"`. When
  supplied, the greedy ordering is computed on the filtered node set.

- individuals:

  Optional individual mapping used when `x` is a data frame or file path
  rather than a prebuilt
  [`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md).

- odgi:

  Optional path to the `odgi` executable. Used when `x` is a raw `.og`
  graph path.

- python:

  Optional path to the Python interpreter. Used when `x` is a raw `.og`
  graph path.

## Value

A character vector of species identifiers in greedy comparison order.

## Examples

``` r
tbl <- data.frame(
  node_id = 1:3,
  sequence = c("A", "C", "G"),
  XZ1516_chromosome = "V_RagTag",
  XZ1516_strand = c("+", "+", "+"),
  XZ1516_absolute_start = c(100L, 101L, 102L),
  XZ1516_absolute_end = c(100L, 101L, 102L),
  N2_chromosome = "V",
  N2_strand = c("+", "+", "NA"),
  N2_absolute_start = c(200L, 201L, "NA"),
  N2_absolute_end = c(200L, 201L, "NA"),
  CB4856_chromosome = "V",
  CB4856_strand = c("+", "NA", "NA"),
  CB4856_absolute_start = c(300L, "NA", "NA"),
  CB4856_absolute_end = c(300L, "NA", "NA"),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

odgi_species_order(tbl, reference_species = "XZ1516")
#> [1] "XZ1516" "N2"     "CB4856"
```
