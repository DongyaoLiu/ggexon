# Rename protein-domain identifiers using an explicit mapping

Rename protein-domain identifiers using an explicit mapping

## Usage

``` r
rename_domain_annotation_ids(
  x,
  mapping,
  annotation = NULL,
  from = NULL,
  to = "transcript_id",
  drop_unmapped = FALSE
)
```

## Arguments

- x:

  A `SynProteinDomainAnnotation` or `SynIndividual` object.

- mapping:

  Either a named character vector (`old_id -> new_id`) or a two-column
  data frame with source and target identifier columns.

- annotation:

  Optional annotation-layer name when `x` is a `SynIndividual`.

- from:

  Source identifier column. Defaults to the domain annotation `keytype`.

- to:

  Target identifier column to populate. Defaults to `"transcript_id"`.

- drop_unmapped:

  Logical; when `TRUE`, drop domain rows that do not map to a target
  identifier.

## Value

The updated object.
