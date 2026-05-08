# ggexon Classes, Geoms, Facets, and Guides

## Overview

`ggexon` separates data storage from plotting. S4 classes remember
genomes, annotation layers, alignments, trees, and layout decisions.
Verbs then load, query, subset, curate, and draw those objects.

The package is easiest to learn in this order:

1.  data-storage classes
2.  plot entry point and geoms
3.  facet systems and genomic guides
4.  verbs that manage, query, and transform stored data

## Data-storage classes

The object model keeps stable biological identifiers for computation
while allowing readable labels and plot-specific layout state to be
stored separately.

``` mermaid
flowchart LR
  sp["SynSpecies<br/>project-level data hub"]
  ind["SynIndividual<br/>one genome, strain, or sample"]
  ann["SynAnnotation<br/>annotation-layer base class"]
  feat["SynFeatureAnnotation<br/>genes, transcripts, exons, CDS"]
  vcf["SynVCFAnnotation<br/>variants"]
  bw["SynBigWigAnnotation<br/>signal tracks"]
  domain["SynProteinDomainAnnotation<br/>protein domains"]
  mut["SynProteinMutationAnnotation<br/>protein mutations"]
  pair["SynPairAlignment<br/>pairwise links"]
  multi["SynMultiAlignment<br/>multi-genome links"]
  layout["SynLayout<br/>panel windows and layout state"]

  sp --> ind
  ind --> ann
  ann --> feat
  ann --> vcf
  ann --> bw
  ann --> domain
  ann --> mut
  sp --> pair
  sp --> multi
  sp --> layout
```

### Project and genome containers

`SynSpecies` is the project-level container. It stores the individuals
being compared, pairwise or multiple alignments between them, optional
trees, and reusable layout state.

``` r

sp <- SynSpecies(name = "Caenorhabditis")
```

`SynIndividual` stores one genome, strain, species, or sample. It can
remember the genome FASTA, feature annotation file, loaded annotation
layers, sequence caches, labels, patches, and feature indexes.

``` r

xz <- SynIndividual(
  genome_file = "XZ1516.fasta",
  annotation_file = "XZ1516.gff3",
  id = "XZ1516"
)
```

At construction time, `ggexon` checks whether sequence names in the
annotation file exist in the FASTA headers.

### Annotation-layer classes

`SynAnnotation` is the shared base class for data layers attached to a
`SynIndividual`.

The main concrete annotation classes are:

| Class | Stores |
|----|----|
| `SynFeatureAnnotation` | Structural genome annotation such as genes, transcripts, exons, CDS, labels, and patch history |
| `SynVCFAnnotation` | Variant data queried by genomic region |
| `SynBigWigAnnotation` | Signal tracks queried by genomic region |
| `SynProteinDomainAnnotation` | Protein-space domains from InterProScan-like tables |
| `SynProteinMutationAnnotation` | Protein mutation summaries and lollipop-track inputs |
| `SynAnnotationPatch` | Small gene-model corrections that add, drop, or replace features |

Additional layers are attached to a `SynIndividual` with
[`add_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_annotation.md).

``` r

xz <- add_annotation(
  xz,
  SynVCFAnnotation(
    name = "variants",
    vcf_file = "sample.vcf.gz"
  )
)

xz <- add_annotation(
  xz,
  SynProteinDomainAnnotation(
    name = "interpro",
    domain_file = "InterProScan.tsv",
    keytype = "protein_id",
    source_db = "InterPro"
  )
)

xz <- add_annotation(
  xz,
  SynBigWigAnnotation(
    name = "coverage",
    bigwig_file = "coverage.bw"
  )
)
```

### Alignment and layout classes

Comparative links are stored at the `SynSpecies` level:

- `SynPairAlignment` stores one pairwise relationship between two
  individuals. Supported inputs include PAF, PSL, and ODGI-derived
  pairwise links.
- `SynMultiAlignment` stores a multiple alignment across more than two
  individuals. Supported inputs include MAF and ODGI-backed graph data.
- `SynLayout` stores reusable panel windows and layout state so plots
  can be rebuilt from the same coordinate decisions.

``` r

sp <- add_individual(sp, xz)

sp <- add_pairwise_alignment(
  sp,
  SynPairAlignment(
    name = "XZ1516_vs_N2",
    query_individual = "XZ1516",
    target_individual = "N2",
    file = "XZ1516_vs_N2.paf"
  )
)

sp <- add_multiple_alignment(
  sp,
  SynMultiAlignment(
    name = "worm-maf",
    individuals = c("XZ1516", "N2", "CB4856"),
    file = "worms.maf"
  )
)
```

## Plot entry point and geoms

[`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md)
starts a plot like `ggplot()`, but it understands `SynIndividual` and
`SynSpecies` objects. Geoms can resolve stored annotation or alignment
data at build time.

``` r

ggexon(sp) +
  geom_exon(
    species = "XZ1516",
    chr = "RagTag_V",
    subset = c(21574445, 21584356)
  )
```

### `geom_exon()`

Use
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md)
when transcript structure matters. It draws exon rectangles, the
transcript backbone, and strand direction from feature annotation data.

``` r

ggexon(sp) +
  geom_exon(
    species = "XZ1516",
    chr = "RagTag_V",
    subset = c(21574445, 21584356)
  )
```

### `geom_exon2()`

Use
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
for exon/CDS/UTR-style tracks with compressed intron display. This is
useful when the visual emphasis is on coding and untranslated segments
rather than raw genomic spacing.

``` r

ggexon(sp) +
  geom_exon2(
    species = "XZ1516",
    chr = "RagTag_V",
    subset = c(21574445, 21584356)
  )
```

### `geom_genetag()`

Use
[`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md)
when each gene should be represented as one directional span. This is a
compact overview layer for synteny figures.

``` r

ggexon(sp) +
  geom_genetag(
    species = "XZ1516",
    chr = "RagTag_V",
    subset = c(21574445, 21584356)
  )
```

### `geom_genelabel()`

Use
[`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md)
to place readable names above or below a gene track. Labels come from
the stored feature annotation, so stable gene IDs can remain unchanged
while plot labels are curated separately.

``` r

ggexon(sp) +
  geom_genetag(species = "XZ1516", chr = "RagTag_V") +
  geom_genelabel(
    species = "XZ1516",
    chr = "RagTag_V",
    label_direction = "top"
  )
```

### `geom_genomic_tree()`

Use
[`geom_genomic_tree()`](https://dongyaoliu.github.io/ggexon/reference/geom_genomic_tree.md)
for genomic tree structures inside ggexon panels. It is intended for
figures where tree-like relationships need to share the same panel
system as genomic intervals.

``` r

ggexon(tree_data) +
  geom_genomic_tree()
```

### `geom_motif()`

Use
[`geom_motif()`](https://dongyaoliu.github.io/ggexon/reference/geom_motif.md)
for motif, domain-like, or other interval blocks. It is a general
interval layer when the data are already in a plot-ready table.

``` r

ggplot(motif_data) +
  geom_motif(aes(xmin = start, xmax = end, y = track, fill = motif))
```

### `geom_mutation_label()`

Use
[`geom_mutation_label()`](https://dongyaoliu.github.io/ggexon/reference/geom_mutation_label.md)
when mutation labels need to be placed on sequence or protein tracks.

``` r

ggplot(mutation_data) +
  geom_mutation_label(aes(x = position, y = track, label = mutation))
```

### `geom_nuclink()`

Use
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
for nucleotide-level links between aligned genomes. With a `SynSpecies`
object, the geom can resolve stored pairwise alignments.

``` r

ggexon(sp) +
  geom_exon(
    species = c("XZ1516", "N2"),
    chr = "RagTag_V",
    subset = c(21574445, 21584356)
  ) +
  geom_nuclink(
    reference = "XZ1516",
    chr = "RagTag_V",
    subset = c(21574445, 21584356),
    alignment = "XZ1516_vs_N2"
  )
```

### `geom_protein_lollipop()`

Use
[`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md)
for protein-domain backbones with mutation lollipops. It is usually
paired with protein-domain or protein-mutation helper data.

``` r

ggplot(protein_lollipop_data) +
  geom_protein_lollipop(
    aes(x = position, y = protein_id, label = mutation)
  )
```

## Facets, tree alignment, and guides

`ggexon` uses facet systems to keep genomic panels, alignment links,
trees, and other plots in register.

### `facet_genomics()`

[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
arranges genomic annotation panels, link panels, signal tracks, and
other data panels in one ggplot-like layout.

``` r

ggexon(sp) +
  geom_exon(species = c("XZ1516", "N2"), chr = "RagTag_V") +
  geom_nuclink(
    reference = "XZ1516",
    chr = "RagTag_V",
    alignment = "XZ1516_vs_N2"
  ) +
  facet_genomics(ggplot2::vars(track), scales = "free_y")
```

### `facet_genomictree()`

[`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md)
is for genomic panels aligned to a tree or another reference plot. Tree
workflows are supported by helpers such as
[`compile_ggtree_genetag()`](https://dongyaoliu.github.io/ggexon/reference/compile_ggtree_genetag.md),
[`compile_ggtree_genomic_alignment()`](https://dongyaoliu.github.io/ggexon/reference/compile_ggtree_genomic_alignment.md),
[`compile_ggtree_rectangular_segments()`](https://dongyaoliu.github.io/ggexon/reference/compile_ggtree_rectangular_segments.md),
and
[`plot_ggtree_genomic_alignment()`](https://dongyaoliu.github.io/ggexon/reference/plot_ggtree_genomic_alignment.md).

``` r

plot_ggtree_genomic_alignment(
  sp,
  alignment = "worm-maf",
  chr = "RagTag_V"
)
```

### Genomic scales and the piecewise guide

[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md)
keeps genomic coordinate labels while supporting compressed regions. The
guide
[`guide_x_ggexon_piecewise()`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md)
shows separate exon and intron scale bars instead of ordinary genomic
ticks.

``` r

ggexon(sp) +
  geom_exon(species = "XZ1516", chr = "RagTag_V") +
  scale_x_ggexon_genomic(
    guide = guide_x_ggexon_piecewise()
  ) +
  facet_genomics(ggplot2::vars(track), scales = "free_y")
```

Use this guide when a figure mixes detailed exon-scale interpretation
with compressed intronic or intergenic distances.

## Verbs that manage stored data

The function families follow one rule: use classes to remember the data,
then use verbs and geoms to operate on those classes.

### Build the object graph

Use `add_*()` functions when a parent object should remember a child
object, annotation layer, alignment, tree, or layout result.

``` r

sp <- SynSpecies(name = "Caenorhabditis") |>
  add_individual(xz, n2) |>
  add_tree(tree)

sp <- add_multiple_alignment(
  sp,
  SynMultiAlignment(
    name = "worm-maf",
    individuals = c("XZ1516", "N2", "CB4856"),
    file = "worms.maf"
  )
)
```

Common graph-building functions include
[`add_individual()`](https://dongyaoliu.github.io/ggexon/reference/add_individual.md),
[`add_individuals_from_folder()`](https://dongyaoliu.github.io/ggexon/reference/add_individuals_from_folder.md),
[`add_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_annotation.md),
[`add_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/add_pairwise_alignment.md),
[`add_multiple_alignment()`](https://dongyaoliu.github.io/ggexon/reference/add_multiple_alignment.md),
[`add_tree()`](https://dongyaoliu.github.io/ggexon/reference/add_tree.md),
[`add_genetag()`](https://dongyaoliu.github.io/ggexon/reference/add_genetag.md),
[`store_chain_layout()`](https://dongyaoliu.github.io/ggexon/reference/store_chain_layout.md),
and
[`store_projected_domains()`](https://dongyaoliu.github.io/ggexon/reference/store_projected_domains.md).

### Load, query, and derive data

Use `load_*()` when the object knows where a file is, but the data have
not yet been materialized.

``` r

xz <- load_annotation(xz)
sp <- load_alignment(sp, alignment = "XZ1516_vs_N2")
```

Use `query_*()` and `*_data()` helpers when you want rows or ranges
back:

``` r

features <- query_features(
  xz,
  chr = "V_RagTag",
  start = 21574445,
  end = 21584356,
  feature_type = "gene"
)

links <- pairwise_alignment_data(
  sp,
  alignment = "XZ1516_vs_N2",
  subset = c(
    XZ1516 = "RagTag_V:21574445-21584356",
    N2 = "V:20456000-20465040"
  )
)
```

Other data verbs include
[`query_variants()`](https://dongyaoliu.github.io/ggexon/reference/query_variants.md),
[`query_signal()`](https://dongyaoliu.github.io/ggexon/reference/query_signal.md),
[`query_domains()`](https://dongyaoliu.github.io/ggexon/reference/query_domains.md),
[`query_protein_mutations()`](https://dongyaoliu.github.io/ggexon/reference/query_protein_mutations.md),
[`extract_cds_seq()`](https://dongyaoliu.github.io/ggexon/reference/extract_cds_seq.md),
[`translate_protein()`](https://dongyaoliu.github.io/ggexon/reference/translate_protein.md),
and
[`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md).

### Modify, subset, and curate objects

Use `subset_*()` and `filter_*()` when you want updated objects or
filtered alignment state.

``` r

sp_window <- subset_species(
  sp,
  coords = c("XZ1516#V_RagTag:21574445-21584356")
)

sp_window <- subset_pairwise_alignment(
  sp_window,
  alignment = "XZ1516_vs_N2",
  subset = c(
    XZ1516 = "RagTag_V:21574445-21584356",
    N2 = "V:20456000-20465040"
  )
)
```

Use curation verbs when a feature annotation needs readable labels or
corrected gene models:

``` r

xz <- set_gene_labels(
  xz,
  c(FUN_000001 = "sept-1", FUN_000002 = "zina-1")
)

xz <- patch_annotation_from_gff(
  xz,
  patch_file = "XZ1516.corrected.gff3",
  mode = "replace",
  name = "manual-curation"
)
```

Patch modes include `replace`, `add`, and `drop`. Patch history can be
inspected with
[`list_patches()`](https://dongyaoliu.github.io/ggexon/reference/list_patches.md)
and cleared with
[`clear_patches()`](https://dongyaoliu.github.io/ggexon/reference/clear_patches.md).

## Recommended workflow

A typical `ggexon` workflow is:

1.  create one or more `SynIndividual` objects
2.  load each active feature annotation
3.  attach optional VCF, BigWig, protein-domain, or mutation layers
4.  set readable plot labels while keeping stable biological IDs
5.  patch corrected gene models if needed
6.  derive CDS, protein, or projected-domain data
7.  collect individuals inside a `SynSpecies`
8.  attach pairwise or multiple alignments
9.  draw geoms and arrange panels with
    [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
    or
    [`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md)
10. use
    [`guide_x_ggexon_piecewise()`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md)
    when compressed genomic axes need explicit exon and intron scale
    bars

## See also

- [`vignette("ggexon-workflow", package = "ggexon")`](https://dongyaoliu.github.io/ggexon/articles/ggexon-workflow.md)
- [`?ggexon`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md)
- [`?facet_genomics`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
- [`?guide_x_ggexon_piecewise`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md)
