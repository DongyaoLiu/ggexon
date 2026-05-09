# Constructor for HomologyAnnotation

Constructor for HomologyAnnotation

## Usage

``` r
HomologyAnnotation(
  name,
  reference_species,
  query_species,
  homology_table,
  source_file = "<homology>",
  metadata = list()
)
```

## Arguments

- name:

  Short unique label for the homology annotation layer.

- reference_species:

  Name of the reference (center) species.

- query_species:

  Name of the query species.

- homology_table:

  A data frame with at minimum `query_gene` and `reference_gene`
  columns.

- source_file:

  Optional path to the source BLAST file.

- metadata:

  Optional metadata list.

## Value

A `HomologyAnnotation` object.
