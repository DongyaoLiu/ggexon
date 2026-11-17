# ggexon

## Genomic visualization across scales

**Grammar of genomic annotations, synteny, and associated data.**

`ggexon` extends ggplot2 to combine gene models, DNA alignments, RNA-seq
coverage, and protein variation in one figure. Prepare the data and
layout in a Syn object, then add the layers you want to inspect.

A `SynIndividual` stores one genome or sample and its annotation layers.
A `SynSpecies` groups individuals with their alignments, homology
mappings, and panel layouts. Syn-aware plotting layers retrieve stored
data for the selected individuals and genomic windows. Scales control
coordinate transformations; themes control appearance.

### **Installation**

`ggexon` is a development package. Install it from GitHub:

``` r
install.packages("remotes")
remotes::install_github("DongyaoLiu/ggexon")
```

If your R installation cannot resolve the Bioconductor dependencies,
install them first:

``` r
install.packages("BiocManager")
BiocManager::install(c(
  "Biostrings", "GenomeInfoDb", "GenomicRanges", "Rsamtools", "rtracklayer"
))
```

BigWig-backed examples run on macOS and Linux. ODGI-backed graph
alignment workflows also require Python 3.8 or newer and `odgi`.

### **From a Syn object to a plot**

Start with the bundled HLA-A reference annotation. Construct and inspect
an individual, add it to a comparison, then draw its exon-intron model:

``` r
library(ggexon)
library(ggplot2)

demo_dir <- system.file("extdata", "hla_a_lollipop", package = "ggexon")
reference <- SynIndividual(
  id = "HLA-A",
  annotation_file = file.path(demo_dir, "hla_a_reference.gff3"),
  annotation_format = "gff"
)
reference <- load_annotation(reference)
exons <- query_features(reference, feature_type = "exon", all = TRUE)

comparison <- SynSpecies(name = "HLA-A reference") |>
  add_individual(reference)

ggexon(comparison) +
  geom_exon2(compress_introns = FALSE) +
  facet_genomics(vars(track)) +
  scale_x_ggexon_genomic(intron_factor = 3) +
  theme_ggexon_track()
```

This example uses allele-local HLA-A coordinates. Exon widths retain
their native scale, and the shared x scale shortens introns three-fold.
The GFF3 is enough for annotation plotting; supply `genome_file` for
sequence extraction or translation.

For comparative figures, attach alignments and signals to the same
object, then store panel windows before plotting. Preparation verbs such
as
[`prepare_genomic_layout()`](https://dongyaoliu.github.io/ggexon/reference/prepare_genomic_layout.md)
and
[`prepare_gene_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/prepare_gene_lollipop.md)
store derived views for reuse. Inspect them with their `query_*()`
counterparts before drawing.

The four tutorials below use bundled data and follow the same sequence:
prepare the Syn object, inspect its contents, and build the plot layer
by layer.

### **1. Projecting amino acid changes onto transcript models**

Project 1,212 amino-acid substitutions from 5,998 eligible named HLA-A
alleles onto the reference protein and its exon-intron model. The
tutorial prepares protein annotations, projects them through the
reference CDS, and stores a shared genomic layout before plotting.
Lollipop heights describe database allele counts, not population
frequencies.

[![HLA-A substitutions projected onto eight coding exons, with introns
compressed three-fold and lollipop heights showing named allele
counts.](articles/hla-a-lollipop-demo_files/figure-html/hla-a-gene-plot-1.png)](https://dongyaoliu.github.io/ggexon/articles/hla-a-lollipop-demo.md)

[Explore the HLA-A protein and gene-model
tutorial](https://dongyaoliu.github.io/ggexon/articles/hla-a-lollipop-demo.md).

### **2. Comparing alternatively spliced transcripts**

Compare human and chimpanzee AS3MT transcripts with a paper-defined
human exon-skipping reconstruction. Build the comparative object first,
then add transcript models, nucleotide ribbons, and aligned DNA at the
exon-1 repeat. One shared x scale compresses introns while preserving
exon widths and the alignment between layers.

[![Human and chimpanzee AS3MT models with aligned repeat sequences and
nucleotide ribbons on an intron-compressed
display.](articles/as3mt-splice-variants-demo_files/figure-html/as3mt-alignseq-1.png)](https://dongyaoliu.github.io/ggexon/articles/as3mt-splice-variants-demo.md)

[Explore the AS3MT transcript and DNA
tutorial](https://dongyaoliu.github.io/ggexon/articles/as3mt-splice-variants-demo.md).

### **3. Visualizing local synteny across species**

Combine conserved flanking genes, a reconstructed three-exon *Dox* model
in *D. simulans*, nucleotide alignments, and testis RNA-seq across three
*Drosophila* species. Prepare annotations, alignments, and combined
RNA-seq tracks in one object, then build the figure layer by layer. Each
assembly retains its genomic axis. RNA panels share one y range and
preserve their signal values; the tutorial identifies the deposited
signals and remapped read depths by their source units.

[![Three Drosophila loci with conserved flanks, an orange Dox model,
native DNA alignments, and combined testis RNA-seq panels with
right-hand y
axes.](articles/dox-synteny-demo_files/figure-html/dox-with-rna-1.png)](https://dongyaoliu.github.io/ggexon/articles/dox-synteny-demo.md)

[Explore the three-species Dox synteny
tutorial](https://dongyaoliu.github.io/ggexon/articles/dox-synteny-demo.md).

### **4. Visualizing megabase-scale haplotypes**

Compare Siamang H1, Gorilla H1, and Human CHM13 across 3.63 Mb MHC
windows.
[`prepare_gene_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/prepare_gene_lollipop.md)
assigns the 58 selected MHC genes to a shared four-row, six-column grid
and stores their native anchors in the Syn object.
[`geom_gene_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_gene_lollipop.md)
retrieves that preparation automatically. Matching gene keys occupy the
same cells, with empty cells reserved. Small native gene arrows, grey
background genes, and direct DNA-alignment ribbons retain the genomic
context below the enlarged heads.

[![Siamang H1, Gorilla H1, and Human CHM13 with 58 gene-lollipop heads
in shared row-and-column slots, native gene spans, grey background
genes, and two DNA-alignment
panels.](articles/figures/mhc-three-species.png)](https://dongyaoliu.github.io/ggexon/articles/mhc-continuous-demo.md)

[Explore the MHC haplotype
tutorial](https://dongyaoliu.github.io/ggexon/articles/mhc-continuous-demo.md).

### Package map

The main containers and preparation functions are:

| Task                          | Objects or functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|-------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Store genomes and annotations | `SynIndividual`, `SynFeatureAnnotation`, `SynBigWigAnnotation`, `SynVCFAnnotation`                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Store protein features        | `SynProteinDomainAnnotation`, `SynProteinMutationAnnotation`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Build comparisons             | `SynSpecies`, `SynPairAlignment`, `SynMultiAlignment`, `HomologyAnnotation`                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| Attach evidence               | [`add_individual()`](https://dongyaoliu.github.io/ggexon/reference/add_individual.md), [`add_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_annotation.md), [`add_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/add_pairwise_alignment.md), [`add_homology_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_homology_annotation.md)                                                                                                                                                 |
| Inspect stored data           | [`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md), [`query_signal()`](https://dongyaoliu.github.io/ggexon/reference/query_signal.md), [`query_variants()`](https://dongyaoliu.github.io/ggexon/reference/query_variants.md), [`query_domains()`](https://dongyaoliu.github.io/ggexon/reference/query_domains.md)                                                                                                                                                                                         |
| Prepare reusable views        | [`project_mutations_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_mutations_to_genome.md), [`prepare_genomic_layout()`](https://dongyaoliu.github.io/ggexon/reference/prepare_genomic_layout.md), [`prepare_gene_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/prepare_gene_lollipop.md)                                                                                                                                                                                                                  |
| Prepare panel layouts         | [`store_chain_layout()`](https://dongyaoliu.github.io/ggexon/reference/store_chain_layout.md), [`set_panel_xlim()`](https://dongyaoliu.github.io/ggexon/reference/set_panel_xlim.md), `SynLayout`, `SynLocusSet`                                                                                                                                                                                                                                                                                                                             |
| Draw models and evidence      | [`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md), [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md), [`geom_gene_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_gene_lollipop.md), [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md), [`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md), [`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md) |
| Set scales and appearance     | [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md), [`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md), [`theme_ggexon_track()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_track.md), [`theme_ggexon_clean()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_clean.md)                                                                                                                                                   |

Use the function reference for supported inputs, selectors, and
additional geoms. Ordinary ggplot2 layers, scales, themes, and
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) remain
available.

### Learn more

Start with [0. Basic
usage](https://dongyaoliu.github.io/ggexon/articles/ggexon-workflow.md)
for data objects, geoms, scales, and panel layouts. Browse the
[tutorials](https://dongyaoliu.github.io/ggexon/articles/index.md) for
the four complete examples, or use the [function
reference](https://dongyaoliu.github.io/ggexon/reference/index.md) for
individual classes and verbs.

The [archived HOX matrix
example](https://github.com/DongyaoLiu/ggexon/blob/HEAD/pkgdown/archive/hox-cluster-expansion-demo.Rmd)
retains the gene-slot comparison, plotting code, and source audit trail.

### Citation

`ggexon` was developed while visualizing the `sept-1`/`zina-1`
toxin-antidote locus. If you use the package in your work, please also
cite:

> Liu D, Zheng C. sept-1/zina-1 is an Ancient Toxin-Antidote System in
> Caenorhabditis elegans. bioRxiv. 2025.
> <https://www.biorxiv.org/content/10.1101/2025.11.28.691152v1>
