# Build a reusable feature lookup index

Builds and stores a lookup index over a loaded feature annotation so
repeated calls to
[`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md)
can resolve seqname, feature-type, gene, and transcript filters without
rescanning the full `GRanges` each time.

## Usage

``` r
build_feature_index(x)
```

## Arguments

- x:

  A `SynIndividual` or `SynFeatureAnnotation` object.

## Value

The updated object.
