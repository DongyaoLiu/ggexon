# Read protein-mutation count summaries

Reads a tab-delimited protein-mutation summary table and normalizes
common columns used by ggexon. Hash notations such as `C#316#H` are
parsed into `ref`, `position`, `alt`, and `mutation` columns.

## Usage

``` r
read_protein_mutation_counts(file, individual_col = "auto", as_long = FALSE)
```

## Arguments

- file:

  Path to a tab-delimited mutation-count table.

- individual_col:

  Column used to identify individuals/strains. `"auto"` checks
  `individual`, `species`, `strain`, `strains`, and `id`, in that order.

- as_long:

  Logical; when `TRUE`, return one row per mutation-individual pair.
  When `FALSE`, return the summary table and store the long index as an
  attribute.

## Value

A normalized `data.frame`.
