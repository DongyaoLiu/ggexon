# Map protein-domain identifiers using an explicit mapping

`map_domain_ids()` maps identifier values into a target column and makes
that column the annotation's matching key. It preserves the source
column when `from` and `to` differ. `rename_domain_annotation_ids()`
remains a compatible synonym with the same arguments and behavior.

## Usage

``` r
map_domain_ids(
  x,
  mapping,
  annotation = NULL,
  from = NULL,
  to = "transcript_id",
  drop_unmapped = FALSE
)

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

## See also

[`query_domains()`](https://dongyaoliu.github.io/ggexon/reference/query_domains.md)
