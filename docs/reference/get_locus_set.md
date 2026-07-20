# Retrieve a locus set from a SynSpecies

Retrieve a locus set from a SynSpecies

## Usage

``` r
get_locus_set(x, name = NULL)
```

## Arguments

- x:

  A `SynSpecies` object.

- name:

  Optional locus-set name. If omitted and exactly one locus set is
  attached, that set is returned.

## Value

A `SynLocusSet` object, or `NULL` when `name` is supplied and absent.
