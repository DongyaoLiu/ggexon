# ggexon

**Grammar of genomic annotations, synteny, and associated data.**

`ggexon` is a ggplot2 extension for drawing genomic annotation together
with comparative and track-level data. It was developed while
visualizing the `sept-1`/`zina-1` toxin-antidote locus described in the
preprint [sept-1/zina-1 is an Ancient Toxin-Antidote System in
Caenorhabditis
elegans](https://www.biorxiv.org/content/10.1101/2025.11.28.691152v1).

The package has two connected parts:

1.  S4 classes store genomes, annotation layers, alignments, and
    reusable layout state.
2.  Functions load, query, modify, and draw those classes with a
    ggplot2-style grammar.

If you already think in `ggplot2`, the goal is that genomic coordinates,
annotation tracks, alignments, and tree-aligned panels can all be
composed with familiar plotting ideas.

## Installation

`ggexon` is currently a development package.

``` r

install.packages("remotes")
remotes::install_github("DongyaoLiu/ggexon")
```

Some dependencies are from Bioconductor. If your R installation cannot
resolve them automatically, install them first:

``` r

install.packages("BiocManager")
BiocManager::install(c(
  "Biostrings",
  "GenomeInfoDb",
  "GenomicRanges",
  "Rsamtools",
  "rtracklayer"
))
```

For ODGI-backed graph alignment workflows, install the system tools
listed in `DESCRIPTION`:

- Python 3.8 or newer
- `odgi`

## Package Map

### Part 1: Classes Store Data

The core data containers are:

| Class | Role |
|----|----|
| `SynSpecies` | Project-level container for individuals, alignments, trees, and layout state |
| `SynIndividual` | One genome, strain, species, or sample |
| `SynFeatureAnnotation` | Genes, transcripts, exons, CDS, labels, and annotation patches |
| `SynVCFAnnotation` | Variant data queried by genomic interval |
| `SynBigWigAnnotation` | Signal tracks queried by genomic interval |
| `SynProteinDomainAnnotation` | Protein-domain intervals from InterProScan-like tables |
| `SynProteinMutationAnnotation` | Protein mutation summaries for lollipop-style tracks |
| `HomologyAnnotation` | Cross-species gene homology mappings from BLAST results |
| `SynPairAlignment` | Pairwise genome links from PAF, PSL, or ODGI-derived data |
| `SynMultiAlignment` | Multi-genome alignment links from MAF or ODGI-backed data |
| `SynLayout` | Stored panel windows and layout decisions |

### Part 2: Functions Manipulate and Draw Classes

The public API is organized around a few function families:

| Task | Main functions |
|----|----|
| Build object graphs | [`add_individual()`](https://dongyaoliu.github.io/ggexon/reference/add_individual.md), [`add_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_annotation.md), [`add_homology_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_homology_annotation.md), [`add_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/add_pairwise_alignment.md), [`add_multiple_alignment()`](https://dongyaoliu.github.io/ggexon/reference/add_multiple_alignment.md), [`add_tree()`](https://dongyaoliu.github.io/ggexon/reference/add_tree.md) |
| Load and query data | [`load_annotation()`](https://dongyaoliu.github.io/ggexon/reference/load_annotation.md), [`load_alignment()`](https://dongyaoliu.github.io/ggexon/reference/load_alignment.md), [`import_blast_homology()`](https://dongyaoliu.github.io/ggexon/reference/import_blast_homology.md), [`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md), [`query_variants()`](https://dongyaoliu.github.io/ggexon/reference/query_variants.md), [`query_signal()`](https://dongyaoliu.github.io/ggexon/reference/query_signal.md), [`query_domains()`](https://dongyaoliu.github.io/ggexon/reference/query_domains.md) |
| Derive sequence/protein data | [`extract_cds_seq()`](https://dongyaoliu.github.io/ggexon/reference/extract_cds_seq.md), [`translate_protein()`](https://dongyaoliu.github.io/ggexon/reference/translate_protein.md), [`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md) |
| Subset and curate objects | [`subset_species()`](https://dongyaoliu.github.io/ggexon/reference/subset_species.md), [`subset_individual()`](https://dongyaoliu.github.io/ggexon/reference/subset_individual.md), [`subset_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/subset_pairwise_alignment.md), [`set_gene_labels()`](https://dongyaoliu.github.io/ggexon/reference/set_gene_labels.md), [`patch_annotation_from_gff()`](https://dongyaoliu.github.io/ggexon/reference/patch_annotation_from_gff.md) |
| Draw genomic layers | [`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md), [`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md), [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md), [`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md), [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md), [`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md) |
| Arrange panels and guides | [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md), [`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md), [`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md), [`guide_x_ggexon_piecewise()`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md) |

The typical workflow is:

``` r

library(ggexon)

x <- SynIndividual(
  genome_file = "genome.fasta",
  annotation_file = "annotation.gff3",
  id = "sample"
)

sp <- SynSpecies(name = "project") |>
  add_individual(x)

ggexon(sp) +
  geom_exon(species = "sample", chr = "chr1")
```

## Learn More

The package site contains the longer cookbook-style documentation:

- [`vignette("ggexon-classes-and-verbs", package = "ggexon")`](https://dongyaoliu.github.io/ggexon/articles/ggexon-classes-and-verbs.md)
  for the class model, every geom, facet systems, and the genomic guide.
- [`vignette("ggexon-workflow", package = "ggexon")`](https://dongyaoliu.github.io/ggexon/articles/ggexon-workflow.md)
  for a step-by-step workflow using individuals, annotations,
  alignments, and direct plotting.
- [`?ggexon`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md)
  and the reference index for function-level documentation.

If you use `ggexon` in work, please also cite the preprint:

> Liu D, Zheng C. sept-1/zina-1 is an Ancient Toxin-Antidote System in
> Caenorhabditis elegans. bioRxiv. 2025.
> <https://www.biorxiv.org/content/10.1101/2025.11.28.691152v1>
