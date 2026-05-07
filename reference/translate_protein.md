# Translate CDS sequences to proteins

Translate CDS sequences to proteins

## Usage

``` r
translate_protein(
  x,
  genes = NULL,
  transcripts = NULL,
  chr = NULL,
  start = NULL,
  end = NULL,
  all = FALSE,
  store = TRUE,
  append = TRUE,
  if.fuzzy.codon = "error"
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

  Logical; when `TRUE`, translate all CDS records.

- store:

  Logical; when `TRUE`, store translated proteins in `protein_seq` and
  the CDS sequences in `nucleotide_seq`.

- append:

  Logical; when `TRUE`, append new sequences to existing cached values
  by name.

- if.fuzzy.codon:

  Passed to
  [`Biostrings::translate()`](https://rdrr.io/pkg/Biostrings/man/translate.html).

## Value

An updated `SynIndividual` object when `store = TRUE`, otherwise an
`AAStringSet`.
