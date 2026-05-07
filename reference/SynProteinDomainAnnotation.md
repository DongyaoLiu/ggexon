# Constructor for SynProteinDomainAnnotation

Constructor for SynProteinDomainAnnotation

## Usage

``` r
SynProteinDomainAnnotation(
  name,
  domain_file,
  keytype = c("protein_id", "transcript_id", "gene_id"),
  source_db = NA_character_,
  metadata = list(),
  lazy = TRUE
)
```

## Arguments

- name:

  Short unique label for the annotation layer.

- domain_file:

  Path to the protein-domain annotation file.

- keytype:

  Key used to map domains to proteins or transcripts.

- source_db:

  Domain database source, such as `"Pfam"` or `"InterPro"`.

- metadata:

  Optional metadata list.

- lazy:

  Logical; whether to defer loading until requested.

## Value

A `SynProteinDomainAnnotation` object.
