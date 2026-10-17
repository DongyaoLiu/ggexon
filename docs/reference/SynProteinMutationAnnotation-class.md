# SynProteinMutationAnnotation class

`SynProteinMutationAnnotation` stores protein-coordinate mutation
records, such as amino-acid substitutions summarized across strains or
individuals. It is a protein-space annotation parallel to
`SynProteinDomainAnnotation`.

## Details

In addition to the slots listed below, the class inherits `name`,
`source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
`plot_cache` from `SynAnnotation`.

## Slots

- `data_format`:

  Mutation file format label. Currently `"protein_mutation"`.

- `mutation_data`:

  Optional pre-populated compatibility store for a normalized mutation
  table. On-demand value-returning queries do not populate the
  caller-owned object.

- `individual_index`:

  Optional long index mapping source mutation rows to individual/strain
  identifiers.

- `keytype`:

  Identifier column used to join mutations to proteins, transcripts, or
  genes.

## Prototype defaults

- `data_format = "protein_mutation"`

- `annotation_scope = "protein"`

- `mutation_data = NULL`

- `individual_index = NULL`

- `keytype = "gene_id"`
