# Constructor for SynAnnotationPatch

Constructor for SynAnnotationPatch

## Usage

``` r
SynAnnotationPatch(
  name,
  patch_data = NULL,
  target_ids = character(),
  mode = c("replace", "add", "remove", "drop"),
  source_file = "<patch>",
  metadata = list()
)
```

## Arguments

- name:

  Patch label.

- patch_data:

  Optional patched gene model as `GRanges`.

- target_ids:

  Target gene IDs to replace, add, or remove.

- mode:

  One of `"replace"`, `"add"`, or `"remove"`. Legacy `"drop"` remains
  supported with the same removal behavior.

- source_file:

  Optional source identifier for the patch. Defaults to `"<patch>"` for
  in-memory patch objects.

- metadata:

  Optional metadata list.

## Value

A `SynAnnotationPatch` object.
