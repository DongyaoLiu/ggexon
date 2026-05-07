# Extract CDS nucleotide sequences

Extract CDS nucleotide sequences

## Usage

``` r
extract_cds_seq(
  x,
  genes = NULL,
  transcripts = NULL,
  chr = NULL,
  start = NULL,
  end = NULL,
  all = FALSE,
  store = TRUE,
  append = TRUE
)
```

## Arguments

- x:

  A `SynIndividual` object.

- genes:

  Optional character vector of gene names or identifiers.

- transcripts:

  Optional character vector of transcript identifiers.

- chr:

  Optional chromosome name.

- start:

  Optional start coordinate.

- end:

  Optional end coordinate.

- all:

  Logical; when `TRUE`, extract all CDS records.

- store:

  Logical; when `TRUE`, store the extracted sequences in
  `nucleotide_seq`.

- append:

  Logical; when `TRUE`, append new sequences to existing cached values
  by name.

## Value

An updated `SynIndividual` object when `store = TRUE`, otherwise a
`DNAStringSet`.
