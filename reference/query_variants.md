# Query variants from a SynVCFAnnotation

For large tabix-backed VCF files, this function uses region-based access
when possible. Plain-text VCF files are read and filtered on demand.

## Usage

``` r
query_variants(x, chr, start, end)
```

## Arguments

- x:

  A `SynVCFAnnotation` object.

- chr:

  Chromosome name.

- start:

  Start coordinate.

- end:

  End coordinate.

## Value

A
[`S4Vectors::DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html)
with the matching variant records.
