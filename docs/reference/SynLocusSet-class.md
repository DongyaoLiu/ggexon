# SynLocusSet class

`SynLocusSet` stores a species-level table of comparable genomic
windows. It is designed for multi-locus or paralog grids where one
`SynSpecies` individual may appear in several panels, one per focal
locus or inferred syntenic window.

## Slots

- `locus_table`:

  A data frame with one row per comparable locus window. Required
  columns are `locus_id`, `individual`, `seqname`, `start`, `end`,
  `row_group`, `col_group`, and `track`.
