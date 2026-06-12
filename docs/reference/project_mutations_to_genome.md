# Project amino-acid variants onto genomic coordinates

Converts protein-coordinate variants (for example `C316H` at residue
316) into genomic coordinates using a transcript's CDS structure, so
amino-acid variants can be annotated directly on the exon/intron model
drawn by
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md).
Each variant is treated as a single codon: residue `p` maps to CDS
nucleotides `(p - 1) * 3 + 1 .. p * 3`, walked across CDS segments so a
codon that spans a splice junction yields one genomic row per segment.
The phase of the 5'-most CDS is honoured for 5'-truncated gene models.

## Usage

``` r
project_mutations_to_genome(
  x,
  annotation = NULL,
  genes = NULL,
  transcripts = NULL,
  strains = NULL,
  mutation = NULL,
  event_type = NULL,
  min_sample_count = NULL,
  protein_ranges = NULL,
  ref = NULL,
  chr = NULL,
  start = NULL,
  end = NULL
)
```

## Arguments

- x:

  A `SynIndividual` object.

- annotation:

  Optional `SynProteinMutationAnnotation` layer name. Defaults to the
  first attached protein-mutation annotation.

- genes, transcripts:

  Optional identifiers limiting the transcripts that variants are
  projected onto.

- strains, mutation, event_type, min_sample_count, protein_ranges, ref:

  Optional variant filters forwarded to
  [`query_protein_mutations()`](https://dongyaoliu.github.io/ggexon/reference/query_protein_mutations.md).

- chr, start, end:

  Optional genomic window used to clip the projection.

## Value

A data frame with one row per (variant, overlapped CDS segment),
containing the projected `seqnames`, `xmin`, `xmax`, `strand`,
`transcripts`, and the variant metadata columns (`position`, `ref`,
`alt`, `mutation`, ...). Returns an empty data frame when nothing
projects.

## Details

This is the mutation counterpart of
[`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md)
and shares the same coordinate-projection core.

## See also

[`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md),
[`geom_aa_variant()`](https://dongyaoliu.github.io/ggexon/reference/geom_aa_variant.md)
