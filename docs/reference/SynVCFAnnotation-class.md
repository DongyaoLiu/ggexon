# SynVCFAnnotation class

`SynVCFAnnotation` stores a region-queryable variant layer backed by a
VCF or BCF file.

## Details

In addition to the slots listed below, the class inherits `name`,
`source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
`plot_cache` from `SynAnnotation`.

## Slots

- `data_format`:

  Variant file format label. Currently `"vcf"`.

- `variants`:

  Optional cached parsed variant data.

- `index_file`:

  Optional index file path for random access.

- `genome_build`:

  Optional genome-build label.

- `region_cache`:

  Cache of previously queried genomic windows.

## Prototype defaults

- `data_format = "vcf"`

- `annotation_scope = "nucleotide"`

- `variants = NULL`

- `index_file = NA_character_`

- `genome_build = NA_character_`

- `region_cache = list()`

## Validity rules

No additional custom validity checks are defined beyond the inherited
slot classes and parent-class rules.
