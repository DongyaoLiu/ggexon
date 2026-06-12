# Annotate amino-acid variants on the exon structure

`geom_aa_variant()` draws protein-coordinate variants (for example
`C316H` at residue 316) at their genomic codon positions, as lollipop
markers sitting above the exon/intron model produced by
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md).
When the plot data is a `SynIndividual` or `SynSpecies`, the layer
resolves an attached `SynProteinMutationAnnotation`, projects each
variant onto its transcript's CDS structure with
[`project_mutations_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_mutations_to_genome.md)
(splice-aware, with codons that cross an intron mapped to the exonic
base), and aligns each marker with the matching transcript row of the
exon layer.

## Usage

``` r
geom_aa_variant(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  species = NULL,
  chr = NULL,
  subset = NULL,
  annotation = NULL,
  genes = NULL,
  transcripts = NULL,
  strains = NULL,
  mutation = NULL,
  event_type = NULL,
  min_sample_count = NULL,
  protein_ranges = NULL,
  ref = NULL,
  exon_height = NULL,
  stem_height = NULL,
  y_offset = NULL,
  stem = NULL,
  spread = NULL,
  spread_threshold = NULL,
  curve_k = NULL,
  stem_colour = NULL,
  stem_linewidth = NULL,
  label_size = NULL,
  label_angle = NULL,
  label_gap = NULL,
  label_colour = NULL,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, ..., na.rm, show.legend, inherit.aes:

  Standard ggplot2 layer arguments.

- species:

  Optional species/individual selector when plotting from a `SynSpecies`
  object.

- chr:

  Optional chromosome name used to define the projection window.

- subset:

  Optional genomic window used to limit projected variants.

- annotation:

  Optional `SynProteinMutationAnnotation` layer name.

- genes, transcripts:

  Optional identifiers limiting the transcripts that variants are
  projected onto.

- strains, mutation, event_type, min_sample_count, protein_ranges, ref:

  Optional variant filters forwarded to
  [`query_protein_mutations()`](https://dongyaoliu.github.io/ggexon/reference/query_protein_mutations.md).

- exon_height:

  Exon rectangle height used to align markers with the exon layer.
  Defaults to `0.8` to match
  [`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md).

- stem_height:

  Height of the lollipop stem above the exon top.

- y_offset:

  Additional vertical offset applied to every marker.

- stem:

  Logical; draw the connecting stem under each head.

- spread:

  Logical; when `TRUE` (default) heads are pushed apart horizontally so
  crowded lollipops do not overlap, while each stem stays anchored at
  the true codon position.

- spread_threshold:

  Minimum horizontal gap (in genomic units) enforced between adjacent
  heads. When `NULL`, a value scaled to the data range is used.

- curve_k:

  Sigmoid steepness of the curved stems linking each anchored base to
  its (possibly spread) head.

- stem_colour, stem_linewidth:

  Stem styling.

- label_size, label_angle, label_gap, label_colour:

  Styling for optional text labels. Labels are drawn at the spread head
  positions when a `label` aesthetic is mapped, e.g.
  `aes(label = mutation)`.

## Value

A ggplot2 layer using
[`GeomAaVariant`](https://dongyaoliu.github.io/ggexon/reference/GeomAaVariant.md).

## Details

The resolved table exposes the variant metadata columns (such as
`position`, `ref`, `alt`, `mutation`, and `sample_count`) so they can
drive aesthetics, e.g. `aes(fill = sample_count)` or
`aes(colour = ref)`.

A plain data frame with `x`/`y` columns can also be supplied for full
manual control.

## See also

[`project_mutations_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_mutations_to_genome.md),
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md),
[`geom_motif()`](https://dongyaoliu.github.io/ggexon/reference/geom_motif.md)
