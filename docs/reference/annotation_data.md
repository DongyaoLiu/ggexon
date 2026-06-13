# Access annotation data

Return the feature-annotation `GRanges` for the active annotation layer
of a `SynIndividual`, or the `GRanges` stored in a
`SynFeatureAnnotation`. This is the read counterpart to
`annotation_data<-`.

## Usage

``` r
annotation_data(x)
```

## Arguments

- x:

  A `SynIndividual` or `SynFeatureAnnotation` object.

## Value

A
[`GenomicRanges::GRanges`](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
object, or `NULL` when no active annotation is present.
