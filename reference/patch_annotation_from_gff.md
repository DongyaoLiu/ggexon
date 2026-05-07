# Apply a patch directly from a small GFF/GTF file

Apply a patch directly from a small GFF/GTF file

## Usage

``` r
patch_annotation_from_gff(
  x,
  patch_file,
  annotation = NULL,
  target_ids = NULL,
  mode = c("replace", "add", "drop"),
  name = NULL,
  format = c("auto", "gff", "gtf")
)
```

## Arguments

- x:

  A `SynFeatureAnnotation` or `SynIndividual` object.

- patch_file:

  Path to the patch GFF/GTF file.

- annotation:

  Optional feature-annotation layer name when `x` is a `SynIndividual`.

- target_ids:

  Optional target gene IDs. When omitted, target IDs are inferred from
  the patch file.

- mode:

  One of `"replace"`, `"add"`, or `"drop"`.

- name:

  Optional patch label.

- format:

  One of `"auto"`, `"gff"`, or `"gtf"`.

## Value

The updated object.
