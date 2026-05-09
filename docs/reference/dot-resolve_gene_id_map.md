# Resolve a gene ID map from a file path or named vector

Accepts either a path to a WormBase-style gene ID mapping file (CSV with
columns `gene_name` and `locus_tag`) or a named character vector.
Returns a named lookup vector `locus_tag → gene_name`.

## Usage

``` r
.resolve_gene_id_map(gene_id_map)
```

## Arguments

- gene_id_map:

  A file path or a named character vector.

## Value

A named character vector.
