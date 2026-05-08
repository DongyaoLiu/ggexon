# Constructor for SynVCFAnnotation

Constructor for SynVCFAnnotation

## Usage

``` r
SynVCFAnnotation(
  name,
  vcf_file,
  index_file = NA_character_,
  genome_build = NA_character_,
  metadata = list(),
  lazy = TRUE
)
```

## Arguments

- name:

  Short unique label for the annotation layer.

- vcf_file:

  Path to the VCF or BCF file.

- index_file:

  Optional index path for large VCF/BCF access.

- genome_build:

  Optional genome build string.

- metadata:

  Optional metadata list.

- lazy:

  Logical; defaults to `TRUE` for region-based querying.

## Value

A `SynVCFAnnotation` object.
