# Build a node-by-node table from an ODGI graph

Runs the bundled Python helper `odgi_node_table.py` against an `.og`
graph and returns either the generated TSV path or the parsed table.

## Usage

``` r
odgi_node_table(
  og_file,
  output = NULL,
  odgi = NULL,
  python = NULL,
  read = TRUE
)
```

## Arguments

- og_file:

  Path to the input ODGI graph (`.og`).

- output:

  Optional output TSV path. Defaults to `<graph>.node_table.tsv` next to
  `og_file`.

- odgi:

  Optional path to the `odgi` executable. If omitted, the helper falls
  back to `ODGI_BIN` and then `odgi` on `PATH`.

- python:

  Optional path to the Python interpreter. Defaults to `python3`, then
  `python`, on `PATH`.

- read:

  Logical; if `TRUE`, read and return the generated TSV as a data frame.
  If `FALSE`, return the output path.

## Value

A data frame when `read = TRUE`, otherwise the output file path.

## Examples

``` r
if (FALSE) { # \dontrun{
tbl <- odgi_node_table("graph.og")

path <- odgi_node_table(
  "graph.og",
  output = "graph.node_table.tsv",
  read = FALSE
)
} # }
```
