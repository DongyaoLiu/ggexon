# Constructor for SynFeatureAnnotation

Constructor for SynFeatureAnnotation

## Usage

``` r
SynFeatureAnnotation(
  name,
  annotation_file,
  annotation_format = "auto",
  metadata = list(),
  lazy = TRUE
)
```

## Arguments

- name:

  Short unique label for the annotation layer.

- annotation_file:

  Path or paths to the GFF or GTF file(s).

- annotation_format:

  One of `"auto"`, `"gff"`, or `"gtf"`, or a vector of the same length
  as `annotation_file`.

- metadata:

  Optional metadata list.

- lazy:

  Logical; whether downstream loading should default to lazy mode.

## Value

A `SynFeatureAnnotation` object with deferred `GRanges`, patch, and
cache slots.
