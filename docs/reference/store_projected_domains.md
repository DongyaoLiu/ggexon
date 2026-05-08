# Store a projected protein-domain table on a SynIndividual

Store a projected protein-domain table on a SynIndividual

## Usage

``` r
store_projected_domains(x, projected, name = "last_projection")
```

## Arguments

- x:

  A `SynIndividual` object.

- projected:

  A data frame returned by
  [`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md).

- name:

  Name used to store the projected table.

## Value

An updated `SynIndividual` object.
