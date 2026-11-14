# Inspect a stored genomic protein projection

Coordinates are original genomic base centres. Mutation anchors use the
longest exonic fragment of each codon. Source changes require projection
again.

## Usage

``` r
query_genomic_projection(
  x,
  annotation,
  component = c("fragments", "anchors"),
  transcripts = NULL,
  chr = NULL,
  start = NULL,
  end = NULL
)
```

## Arguments

- x:

  A SynIndividual containing a stored projection.

- annotation:

  Name of a stored genomic projection.

- component:

  `"fragments"` or `"anchors"`.

- transcripts, chr, start, end:

  Optional genomic/transcript filters.

## Value

A data frame; the source object is unchanged.
