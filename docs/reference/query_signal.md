# Query signal from a SynBigWigAnnotation

Query signal from a SynBigWigAnnotation

## Usage

``` r
query_signal(x, chr, start, end)
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

## Value

A `GRanges` object with the overlapping signal records.
