# Normalize a gene identifier for symmetric matching

Applies the same normalization to both BLAST query IDs and annotation
feature IDs so they can be matched reliably. Normalization steps:

1.  Trim whitespace

2.  Strip common prefixes (gene:, transcript:, cds:, mRNA:)

3.  Strip transcript isoform suffixes (.t1 .t2 ... -T1 -T2 ...)

4.  Strip locus-tag isoform letters (B0250.18a → B0250.18)

## Usage

``` r
.normalize_gene_id(x)
```

## Arguments

- x:

  Character vector of gene identifiers.

## Value

A normalized character vector.
