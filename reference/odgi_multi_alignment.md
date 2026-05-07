# Convert an ODGI node table into a `SynMultiAlignment`

Accepts either an in-memory node table returned by
[`odgi_node_table()`](https://dongyaoliu.github.io/ggexon/reference/odgi_node_table.md),
a path to a TSV written by the bundled helper, or a raw `.og` ODGI graph
path. File-backed inputs are converted to the node-table representation,
validated, and stored on a `SynMultiAlignment` with `format = "odgi"`.

## Usage

``` r
odgi_multi_alignment(
  x,
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

  A data frame, an ODGI node-table TSV path, or an `.og` graph path.

- name:

  Optional alignment label. Defaults to the file stem when `x` is a
  path, otherwise `"odgi-alignment"`.

- individuals:

  Optional character vector/list describing which `SynIndividual`
  identifiers correspond to the ODGI path labels. If named, the names
  must match the path labels in the table.

- odgi:

  Optional path to the `odgi` executable. Used when `x` is an `.og`
  graph file.

- python:

  Optional path to the Python interpreter. Used when `x` is an `.og`
  graph file.

- file:

  Optional source file to store on the returned object. Defaults to `x`
  when `x` is a path, otherwise `"<odgi-node-table>"`.

- metadata:

  Optional metadata list.

## Value

A `SynMultiAlignment` object with `format = "odgi"` and the parsed table
cached in its `data` slot.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- odgi_node_table("graph.og")
msa <- odgi_multi_alignment(tbl, name = "graph-msa")

msa2 <- odgi_multi_alignment(
  "graph.node_table.tsv",
  individuals = c(XZ1516 = "XZ1516", N2 = "N2")
)
} # }
```
