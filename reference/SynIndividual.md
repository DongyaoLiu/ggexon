# Constructor for SynIndividual

Constructor for SynIndividual

## Usage

``` r
SynIndividual(
  genome_file = genome_waiver(),
  annotation_file = NA_character_,
  id = NULL,
  annotation_format = "auto",
  metadata = list()
)
```

## Arguments

- genome_file:

  Path to the genome FASTA file. Use
  [`genome_waiver()`](https://dongyaoliu.github.io/ggexon/reference/genome_waiver.md)
  to initialize a `SynIndividual` without a genome FASTA.

- annotation_file:

  Optional path or paths to the corresponding GFF or GTF file(s).

- id:

  Optional scalar identifier. Defaults to the FASTA stem, or to the
  first annotation-file stem when `genome_file` is waived. Required when
  neither file input is supplied.

- annotation_format:

  One of `"auto"`, `"gff"`, or `"gtf"`, or a vector of the same length
  as `annotation_file`.

- metadata:

  Optional metadata list.

## Value

A `SynIndividual` object with deferred slots left empty.
