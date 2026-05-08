# SynFeatureAnnotation class

`SynFeatureAnnotation` stores structural genome annotation such as
genes, transcripts, exons, CDS, and derived label or sequence caches. It
is the default feature-layer type attached to `SynIndividual`.

## Details

In addition to the slots listed below, the class inherits `name`,
`source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
`plot_cache` from `SynAnnotation`.

## Slots

- `annotation_format`:

  Source annotation format. One of `"auto"`, `"gff"`, or `"gtf"`.

- `base_annotation`:

  Optional immutable base `GRanges` before any patches are applied.

- `annotation`:

  Optional active `GRanges` after patching or label updates.

- `patches`:

  List of `SynAnnotationPatch` objects applied to this feature layer.

- `feature_index`:

  Optional cached lookup structure for features.

- `label_map`:

  Optional cached feature-id to display-label mapping.

- `nucleotide_seq`:

  Optional cached nucleotide sequences extracted from the annotated
  features.

- `protein_seq`:

  Optional cached translated protein sequences.

## Prototype defaults

- `annotation_format = "auto"`

- `annotation_scope = "nucleotide"`

- `base_annotation = NULL`

- `annotation = NULL`

- `patches = list()`

- `feature_index = NULL`

- `label_map = NULL`

- `nucleotide_seq = NULL`

- `protein_seq = NULL`

## Validity rules

- `annotation_format` must be one of `"auto"`, `"gff"`, or `"gtf"`,
  either length one or the same length as `source_file`.
