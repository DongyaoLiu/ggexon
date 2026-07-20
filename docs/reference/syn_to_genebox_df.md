# Prepare gene-box anchors from Syn feature annotations

`syn_to_genebox_df()` is the data compiler used by
[`geom_genebox()`](https://dongyaoliu.github.io/ggexon/reference/geom_genebox.md)
for Syn-backed layers. It is also useful when the selected transcript
and anchor provenance need to be inspected or displayed as a table.

## Usage

``` r
syn_to_genebox_df(
  x,
  species = NULL,
  chr = NULL,
  subset = NULL,
  anchor = c("middle", "start", "end"),
  na.rm = FALSE,
  context = NULL
)
```

## Arguments

- x:

  A
  [SynIndividual](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
  or
  [SynSpecies](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  object.

- species:

  Optional individual selector for `SynSpecies` input.

- chr:

  Optional chromosome or seqname.

- subset:

  Optional numeric length-two genomic window.

- anchor:

  One of `"middle"`, `"start"`, or `"end"`.

- na.rm:

  Suppress the summary warning about transcripts without usable CDS
  anchors.

- context:

  Internal Syn plotting context.

## Value

A data frame with one selected protein-coding transcript per gene.
Per-end source and fallback columns distinguish complete, exactly
three-nucleotide codon features from terminal-CDS positional proxies.
