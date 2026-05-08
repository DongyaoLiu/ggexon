# Query protein-domain annotations

Query protein-domain annotations

## Usage

``` r
query_domains(x, ids = NULL, domains = NULL)
```

## Arguments

- x:

  A `SynProteinDomainAnnotation` object.

- ids:

  Optional identifiers to match against `keytype`.

- domains:

  Optional domain names to filter.

## Value

A
[`S4Vectors::DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
with matching domain records.
