# Retrieve a HomologyAnnotation from a SynSpecies by query species

When `query_species` is supplied, the first `HomologyAnnotation` whose
`query_species` matches is returned. When `name` is supplied, the
annotation with that exact name is returned. Provide one or the other.

## Usage

``` r
get_homology_annotation(x, query_species = NULL, name = NULL)
```

## Arguments

- x:

  A `SynSpecies` object.

- query_species:

  Optional name of the query species.

- name:

  Optional name of the homology annotation layer.

## Value

A `HomologyAnnotation` object, or `NULL` when not found.
