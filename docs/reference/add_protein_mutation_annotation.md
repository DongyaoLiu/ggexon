# Add protein-mutation annotations

Attaches protein-coordinate mutation summaries to a `SynIndividual`, or
dispatches them across child individuals of a `SynSpecies` when an
individual/strain column is available.

## Usage

``` r
add_protein_mutation_annotation(
  x,
  mutation_file,
  name = "protein_mutations",
  keytype = "gene_id",
  individual = NULL,
  individual_col = "auto",
  all = TRUE,
  create_missing = TRUE,
  metadata = list()
)
```

## Arguments

- x:

  A `SynIndividual` or `SynSpecies` object.

- mutation_file:

  Path to a protein-mutation count table.

- name:

  Annotation-layer name.

- keytype:

  Identifier column used to match mutation rows to features.

- individual:

  Optional individual name(s). For a `SynIndividual`, this defaults to
  `syn_id(x)`. For a `SynSpecies`, this limits the import.

- individual_col:

  Individual/strain column, or `"auto"`.

- all:

  Logical. For `SynSpecies`, when `TRUE`, import all routed individuals
  by default.

- create_missing:

  Logical. For `SynSpecies`, create annotation-only `SynIndividual`
  objects for routed individuals that are not already stored.

- metadata:

  Optional metadata list.

## Value

The updated `SynIndividual` or `SynSpecies`.
