# SynSpecies, SynLayout, and alignment classes

These classes define the comparative object model used by `ggexon`.
`SynSpecies` groups multiple `SynIndividual` objects, `SynPairAlignment`
and `SynMultiAlignment` store the relationships between those
individuals as species-level annotations, and `SynLayout` stores
reusable panel-layout metadata for plotting.

## Class overview

- `SynPairAlignment`: one pairwise alignment between two individuals

- `SynMultiAlignment`: one multiple alignment covering several
  individuals

- `SynLocusSet`: one table of comparable locus windows for grid layouts

- `SynLayout`: panel layout plus shared layout-scoped plotting defaults

- `SynSpecies`: top-level container that binds individuals, alignments,
  and an optional stored layout
