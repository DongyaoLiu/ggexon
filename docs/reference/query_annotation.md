# Query a region-backed annotation

Query a region-backed annotation

## Usage

``` r
query_annotation(x, region, ...)

# S4 method for class 'SynBigWigAnnotation'
query_annotation(x, region, ...)
```

## Arguments

- x:

  A region-backed `SynAnnotation` object.

- region:

  A length-one
  [`GenomicRanges::GRanges`](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  query.

- ...:

  Additional arguments passed to a class method.

## Value

A class-specific region query result.
