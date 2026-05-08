# Attach an InterProScan protein-domain layer to a SynIndividual

Attach an InterProScan protein-domain layer to a SynIndividual

## Usage

``` r
add_interproscan_annotation(
  x,
  domain_file = system.file("extdata", "InterProScan.tsv", package = "ggexon"),
  name = "interpro",
  keytype = c("protein_id", "transcript_id", "gene_id"),
  source_db = "InterPro"
)
```

## Arguments

- x:

  A `SynIndividual` object.

- domain_file:

  Path to an InterProScan TSV export. Defaults to the bundled
  `InterProScan.tsv` example when available.

- name:

  Annotation-layer name used to store the imported domains.

- keytype:

  Identifier column used to match domain rows to proteins or
  transcripts.

- source_db:

  Domain database label recorded in the annotation metadata.

## Value

An updated `SynIndividual` object with a `SynProteinDomainAnnotation`
layer attached.
