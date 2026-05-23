# HomologyAnnotation class

`HomologyAnnotation` stores cross-species gene homology mappings derived
from BLAST results. It is a species-level annotation attached to
`SynSpecies` via the `homology_annotations` slot.

## Details

Each object maps genes from one query species to genes in a reference
species (typically the best-annotated "center" species). The mapping is
automatically injected into all geom data frames (`geom_exon`,
`geom_gene`, `geom_genetag`, `geom_genelabel`) when the
`HomologyAnnotation` is attached to a `SynSpecies`. Two new columns
`reference_gene` and `reference_gene_name` become available for mapping
in ggplot2 aesthetics.

## Slots

- `reference_species`:

  Scalar name of the reference species (e.g., `"C. elegans N2"`).

- `query_species`:

  Scalar name of the query species whose genes map to the reference.

- `homology_table`:

  A data frame with at minimum the columns `query_gene` and
  `reference_gene`.

## Prototype defaults

- `annotation_scope = "species"`

- `lazy = TRUE`

- `loaded = TRUE`

- `reference_species = NA_character_`

- `query_species = NA_character_`

- `homology_table = data.frame(query_gene = character(), reference_gene = character())`

## Validity rules

- `reference_species` and `query_species` must each be one non-empty
  character value and must differ.

- `homology_table` must be a data frame containing at least `query_gene`
  and `reference_gene` columns.

- `homology_table$query_gene` must be unique.
