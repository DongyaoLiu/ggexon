# Check whether genome and annotation files match

Validates that every sequence name in the first column of the annotation
file is present among the FASTA headers. FASTA names are compared using
the first token after `>`, which matches standard GFF/GTF `seqname`
usage.

## Usage

``` r
check_syn_files(genome_file, annotation_file)
```

## Arguments

- genome_file:

  Path to the genome FASTA file.

- annotation_file:

  Path to the corresponding GFF or GTF file.

## Value

Invisibly returns `TRUE` when the files match.
