# Query BigWig signal

Query BigWig signal

## Usage

``` r
query_signal(x, chr, start, end, ...)
```

## Arguments

- x:

  A `SynBigWigAnnotation` object.

- chr:

  Chromosome name.

- start:

  Start coordinate.

- end:

  End coordinate.

- ...:

  Additional arguments passed to
  [`query_annotation()`](https://dongyaoliu.github.io/ggexon/reference/query_annotation.md).

## Value

A
[`GenomicRanges::GRanges`](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
object with overlapping signal records.
