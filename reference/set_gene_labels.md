# Set human-readable gene labels on a feature annotation layer

This keeps stable gene IDs for internal logic and adds a `plot_label`
metadata column for plotting.

## Usage

``` r
set_gene_labels(x, mapping, annotation = NULL)
```

## Arguments

- x:

  A `SynFeatureAnnotation` or `SynIndividual` object.

- mapping:

  Either a named character vector (`feature_id -> label`) or a
  two-column data frame with ID and label columns.

- annotation:

  Optional feature-annotation layer name when `x` is a `SynIndividual`.

## Value

The updated object.
