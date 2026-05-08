# SynAnnotation class

`SynAnnotation` is the abstract base class for all annotation layers
attached to `SynIndividual` or `SynSpecies` objects. Subclasses
specialize the owner level, coordinate system, and payload they carry,
but all annotations share the same naming, source-file, lazy loading,
metadata, and plot-cache semantics.

## Slots

- `name`:

  Short unique label used to retrieve the annotation layer.

- `source_file`:

  Path or paths to the on-disk file(s) backing the annotation.

- `annotation_scope`:

  Scalar coordinate scope such as `"nucleotide"` or `"protein"`.

- `lazy`:

  Logical; whether data loading should normally be deferred.

- `loaded`:

  Logical; whether the payload has been materialized in memory.

- `metadata`:

  Optional user or import metadata.

- `plot_cache`:

  List used to store derived plotting tables or other reusable
  render-time state.

## Prototype defaults

- `annotation_scope = "unknown"`

- `lazy = TRUE`

- `loaded = FALSE`

- `metadata = list()`

- `plot_cache = list()`

## Validity rules

- `name` and `annotation_scope` must each be one non-empty character
  value.

- `source_file` must be a non-empty character vector with no empty
  entries.

- `lazy` and `loaded` must each be scalar logical values.
