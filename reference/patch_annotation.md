# Apply a gene-model patch to a feature annotation

Apply a gene-model patch to a feature annotation

## Usage

``` r
patch_annotation(
  x,
  patch,
  annotation = NULL,
  target_ids = NULL,
  mode = c("replace", "add", "drop"),
  name = NULL
)
```

## Arguments

- x:

  A `SynFeatureAnnotation` or `SynIndividual` object.

- patch:

  A `SynAnnotationPatch`, `GRanges`, or patch-like data.

- annotation:

  Optional feature-annotation layer name when `x` is a `SynIndividual`.

- target_ids:

  Optional target gene IDs when `patch` is not already a
  `SynAnnotationPatch`.

- mode:

  One of `"replace"`, `"add"`, or `"drop"`.

- name:

  Optional patch label.

## Value

The updated object.
