# Coerce and assign annotation data

Coerce and assign annotation data

## Usage

``` r
annotation_data(x) <- value

# S4 method for class 'SynIndividual'
annotation_data(x) <- value

# S4 method for class 'SynFeatureAnnotation'
annotation_data(x) <- value
```

## Arguments

- x:

  A `SynIndividual` object.

- value:

  A
  [`GenomicRanges::GRanges`](https://rdrr.io/pkg/GenomicRanges/man/GRanges-class.html)
  object or `NULL`.

## Value

The updated `SynIndividual` object.

## Details

For a `SynIndividual`, `active_feature_annotation(x)` must name an
attached `SynFeatureAnnotation` layer before replacement. Add a feature
layer with
[`add_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_annotation.md)
first. Replacing with `NULL` clears the active feature layer and its
synchronized owner state.
