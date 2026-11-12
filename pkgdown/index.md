# ggexon

<style>
#main h3 {
  font-weight: 500;
}
</style>

## Genomic visualization across scales

Understanding genomic variation often means moving between individual bases,
transcripts, genes, and larger regions shared across genomes. `ggexon` extends
ggplot2 so you can connect these views in static figures, combining gene
models, nucleotide alignments, RNA-seq coverage, and protein features through
a familiar layered plotting grammar.

The examples below progress from amino acid substitutions within one gene to
synteny across megabase-sized haplotypes. Each figure links to a tutorial with
the data, plotting steps, and guidance for interpreting the result.

### **Installation**

`ggexon` is currently a development package. Install it from GitHub:

```r
install.packages("remotes")
remotes::install_github("DongyaoLiu/ggexon")
```

If your R installation cannot resolve the Bioconductor dependencies
automatically, install them first:

```r
install.packages("BiocManager")
BiocManager::install(c(
  "Biostrings", "GenomeInfoDb", "GenomicRanges", "Rsamtools", "rtracklayer"
))
```

BigWig-backed examples run on macOS and Linux. ODGI-backed graph alignment
workflows additionally require Python 3.8 or newer and `odgi`.

### **1. Projecting amino acid changes onto transcript models**

Project protein variation onto an exon-intron model to see where substitutions
fall within the coding sequence. This example places 1,212 substitutions from
5,998 eligible named HLA-A alleles onto the reference gene, preserving exon
widths while shortening introns. Lollipop heights show database allele counts,
not population frequencies.

<a href="articles/hla-a-lollipop-demo.html"><img src="man/figures/hla-a-genomic-lollipop-demo.png" alt="HLA-A amino acid substitutions projected onto eight coding exons, with lollipop heights showing named allele counts and introns compressed three-fold." width="100%" style="background-color: white;"></a>

[Explore the HLA-A protein and gene-model tutorial](articles/hla-a-lollipop-demo.html).

### **2. Comparing alternatively spliced transcripts**

Compare human and chimpanzee AS3MT gene models alongside a paper-defined human
exon-skipping reconstruction. The combined view connects transcript structure,
aligned DNA at an exon-intron junction, RNA-seq coverage, and counted splice
junctions. Exon widths are preserved while introns are compressed, keeping
sequence detail aligned with the surrounding gene structure.

<a href="articles/as3mt-splice-variants-demo.html"><img src="articles/as3mt-splice-variants-demo_files/figure-html/as3mt-alignseq-1.png" alt="Human and chimpanzee AS3MT models with aligned repeat sequences, nucleotide ribbons, and human RNA-seq coverage and splice-junction counts on a shared intron-compressed display." width="100%" loading="lazy" style="background-color: white;"></a>

[Explore the AS3MT transcript, DNA, and RNA-seq tutorial](articles/as3mt-splice-variants-demo.html).

### **3. Visualizing local synteny across species**

Place a gene in its surrounding genomic context. This three-species
*Drosophila* comparison combines conserved flanking genes, an additional
*Dox* coding model in *D. simulans*, nucleotide-alignment ribbons, and testis
RNA-seq tracks. The tutorial builds the figure layer by layer, showing how
gene order, exon structure, and transcriptional signal can be inspected
together. RNA panels use independent y scales and retain their source units.

<a href="articles/dox-synteny-demo.html"><img src="man/figures/dox-three-species-demo.png" alt="Three Drosophila loci with conserved flanking genes, an additional Dox model in D. simulans, nucleotide-alignment ribbons, and separate testis RNA-seq tracks." width="100%" loading="lazy" style="background-color: white;"></a>

[Explore the three-species Dox synteny tutorial](articles/dox-synteny-demo.html).

### **4. Visualizing megabase-scale haplotypes**

Compare gene organization and sequence alignments across the major
histocompatibility complex (MHC). This compact view shows human CHM13, two
gorilla haplotypes, and one siamang haplotype on a common base-pair scale.
Small arrows retain gene spans and direction; enlarged labels identify MHC
genes above the surrounding annotations and alignment ribbons. The tutorial
also includes the complete thirteen-track comparison across apes.

<a href="articles/mhc-continuous-demo.html"><img src="articles/figures/mhc-three-species.png" alt="Four megabase-scale MHC haplotypes from siamang, gorilla, and human, with native gene spans, enlarged gene labels, variable-region annotations, and nucleotide-alignment ribbons." width="100%" loading="lazy" style="background-color: white;"></a>

[Explore the MHC haplotype tutorial](articles/mhc-continuous-demo.html).

### Build your own figure

Start with [0. Basic usage](articles/ggexon-workflow.html) to prepare
data and draw your first genomic tracks. A `SynIndividual` stores one genome
or sample and its annotation layers; a `SynSpecies` groups individuals with
their comparative relationships. Plotting layers retrieve the data they need
from these containers, or use prepared tables directly.

Use the [function reference](reference/index.html) to choose geoms, facets,
and scales. Browse the four [tutorials](articles/index.html) for protein
variation, alternative splicing, local synteny, and megabase-scale haplotypes.
