# Filter ODGI nodes by sequence length

Loads an ODGI-backed
[`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md)
if needed, then keeps only nodes whose sequence length satisfies a
comparison such as `"> 10"` or "`<= 3`". The returned object keeps the
same alignment metadata and ODGI label mapping but caches the filtered
node table in memory.

## Usage

``` r
filter_odgi_nodes(
  x,
  filter_by_len,
  alignment = NULL,
  odgi = NULL,
  python = NULL
)
```

## Arguments

- x:

  A
  [`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md)
  with `format = "odgi"` or a
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  object containing one or more ODGI multiple alignments.

- filter_by_len:

  A single comparison string such as `"> 10"`, `"= 3"`, `"< 5"`,
  `">= 8"`, or `"<= 2"`.

- alignment:

  Optional multiple-alignment name when `x` is a
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md).
  If omitted and exactly one multiple alignment is stored, that
  alignment is used.

- odgi:

  Optional path to the `odgi` executable. Used when an ODGI alignment is
  backed by a raw `.og` graph and must be loaded first.

- python:

  Optional path to the Python interpreter. Used when an ODGI alignment
  is backed by a raw `.og` graph and must be loaded first.

## Value

An updated object of the same class as `x`.

## Details

When called on a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
object, `alignment` selects which stored multiple alignment to update.
The returned `SynSpecies` contains the filtered `SynMultiAlignment` in
place.

## Examples

``` r
tbl <- data.frame(
  node_id = 1:2,
  sequence = c("AC", "G"),
  XZ1516_chromosome = c("V_RagTag", "V_RagTag"),
  XZ1516_strand = c("+", "-"),
  XZ1516_absolute_start = c(100L, 102L),
  XZ1516_absolute_end = c(101L, 102L),
  N2_chromosome = c("V", "V"),
  N2_strand = c("+", "+"),
  N2_absolute_start = c(200L, 202L),
  N2_absolute_end = c(201L, 202L),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

msa <- odgi_multi_alignment(tbl, name = "worm-graph")
filter_odgi_nodes(msa, "> 1")
#> An object of class "SynMultiAlignment"
#>   name: worm-graph 
#>   source_file: <odgi-node-table> 
#>   annotation_scope: species 
#>   lazy: FALSE 
#>   loaded: TRUE 
```
