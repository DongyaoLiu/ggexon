# SynAnnotationPatch class

`SynAnnotationPatch` records a patch that replaces, adds, or drops a
subset of features from a `SynFeatureAnnotation`. As a genome-coordinate
annotation, it inherits the shared `SynGenomeAnnotation` metadata used
across nucleotide-level annotation layers.

## Details

In addition to the slots listed below, the class inherits `name`,
`source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
`plot_cache` from `SynAnnotation`.

## Slots

- `name`:

  Patch label.

- `source_file`:

  Optional source identifier describing where the patch came from.
  In-memory patches default to `"<patch>"`.

- `patch_data`:

  Optional patch payload as `GRanges`. Required for `"replace"` and
  `"add"` patches.

- `target_ids`:

  Character vector of feature identifiers targeted by the patch.

- `mode`:

  Patch mode. One of `"replace"`, `"add"`, or `"drop"`.

- `metadata`:

  Optional patch metadata.

## Prototype defaults

- `source_file = "<patch>"`

- `annotation_scope = "nucleotide"`

- `lazy = FALSE`

- `loaded = TRUE`

- `patch_data = NULL`

- `target_ids = character()`

- `mode = "replace"`

## Validity rules

- `mode` must be one of `"replace"`, `"add"`, or `"drop"`.

- `patch_data` is required when `mode` is `"replace"` or `"add"`.
