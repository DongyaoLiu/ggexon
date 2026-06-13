# Retrieve or replace the homology table from a HomologyAnnotation

`homology_table()` returns the current table. `homology_table<-`
replaces the full table after applying the same normalization rules as
[`HomologyAnnotation()`](https://dongyaoliu.github.io/ggexon/reference/HomologyAnnotation.md):
`query_gene` and `reference_gene` are coerced to character, incomplete
rows are dropped, duplicated `query_gene` values warn, and the first row
for each duplicated query is kept.

## Usage

``` r
homology_table(x)

# S4 method for class 'HomologyAnnotation'
homology_table(x)

homology_table(x) <- value

# S4 method for class 'HomologyAnnotation'
homology_table(x) <- value
```

## Arguments

- x:

  A `HomologyAnnotation` object.

- value:

  A data frame with at least `query_gene` and `reference_gene`.

## Value

A data frame with `query_gene`, `reference_gene`, and any extra homology
metadata columns.

The updated `HomologyAnnotation` object.
