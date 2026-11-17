# Inspect shared prepared genomic coordinates

Inspect shared prepared genomic coordinates

## Usage

``` r
query_genomic_layout(
  x,
  name,
  component = c("mutations", "stems", "exons", "backbone", "domains")
)
```

## Arguments

- x:

  A SynIndividual containing a prepared genomic layout.

- name:

  Prepared layout name.

- component:

  One of mutations, stems, exons, backbone, or domains.

## Value

A data frame in original genomic coordinates.
