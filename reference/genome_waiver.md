# Genome-file waiver for `SynIndividual()`

Use this helper when you want to initialize a `SynIndividual` from
annotations only, without an available genome FASTA. Sequence-dependent
operations such as
[`extract_cds_seq()`](https://dongyaoliu.github.io/ggexon/reference/extract_cds_seq.md)
and
[`translate_protein()`](https://dongyaoliu.github.io/ggexon/reference/translate_protein.md)
will then stop with a clear error message.

## Usage

``` r
genome_waiver()
```

## Value

A sentinel value understood by
[`SynIndividual()`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md).
