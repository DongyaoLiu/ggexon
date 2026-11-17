# Inspect a prepared gene-lollipop grid

Source changes and cleared caches require preparation again. Slot
occupancy describes selected annotation features and does not establish
gene absence. An extracted individual retains the complete shared grid.

## Usage

``` r
query_gene_lollipop(x, name = NULL, component = c("genes", "slots"))
```

## Arguments

- x:

  A SynIndividual or SynSpecies containing a preparation.

- name:

  Optional preparation name; inferred only when unambiguous.

- component:

  Return native `"genes"` or the common `"slots"` table.

## Value

A data frame. `occupied_by` is a list column of individual IDs.
