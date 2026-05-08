# Constructor for SynProteinMutationAnnotation

Constructor for SynProteinMutationAnnotation

## Usage

``` r
SynProteinMutationAnnotation(
  name,
  mutation_file,
  keytype = "gene_id",
  mutation_data = NULL,
  individual_index = NULL,
  metadata = list(),
  lazy = TRUE
)
```

## Arguments

- name:

  Short unique label for the annotation layer.

- mutation_file:

  Path to the protein-mutation annotation file.

- keytype:

  Identifier column used to match mutation rows to features.

- mutation_data:

  Optional normalized mutation table.

- individual_index:

  Optional long table mapping mutation rows to individual identifiers.

- metadata:

  Optional metadata list.

- lazy:

  Logical; whether to defer loading until requested.

## Value

A `SynProteinMutationAnnotation` object.
