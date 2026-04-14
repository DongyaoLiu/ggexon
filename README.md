# ggexon

`ggexon` is an R package for plotting exon structures, genome tracks, and
synteny-aware comparative views with a `ggplot2`-style interface.

It combines grammar-of-graphics plotting with object-based containers for
genomes, annotations, and alignments, so you can move from raw genome files to
reproducible comparative figures without building a custom plotting workflow
from scratch.

## Why `ggexon`

`ggexon` is built for genomics workflows where plain `ggplot2` is flexible but
too low-level, and where you want tighter control than browser-style genome
viewers usually provide.

Use `ggexon` when you need to:

- visualize exon and transcript structures in specific genomic windows
- attach multiple annotation layers to the same genome object
- preserve stable biological IDs while plotting readable gene labels
- patch corrected gene models from small GFF fragments
- query and plot pairwise or multi-genome synteny context

The core data model is organized around:

- `SynIndividual`: one genome plus its attached annotations
- `SynSpecies`: a collection of individuals and their alignments
- `SynAnnotation` subclasses: feature, variant, signal, and protein-domain
  layers

## Installation

`ggexon` is currently a development package. If you are working from a local
checkout, install the required CRAN and Bioconductor dependencies first, then
install the package:

```r
install.packages(c(
  "ggplot2", "dplyr", "tidyr", "rlang", "vctrs", "S7",
  "gtable", "ggforce", "ggridges", "reshape2", "scales"
))

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install(c(
  "Biostrings", "GenomeInfoDb", "GenomicRanges", "rtracklayer"
))

remotes::install_local(".")
```

If you later publish the package on GitHub or Bioconductor, this section can be
replaced with a simpler one-line install command.

## Quick Start

The minimal workflow is:

1. Create a `SynIndividual` from a genome FASTA and annotation file
2. Load the active feature annotation
3. Optionally add readable labels or extra annotation layers
4. Plot a genomic region with a `ggplot2`-style call

Create a genome-level container from a FASTA file and a structural annotation:

```r
library(ggexon)

x <- SynIndividual(
  genome_file = system.file("extdata", "XZ1516.fasta", package = "ggexon"),
  annotation_file = system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  ),
  id = "XZ1516"
)

x <- load_annotation(x)
```

Add readable labels for plotting while keeping stable IDs in the underlying
annotation:

```r
x <- set_gene_labels(
  x,
  c(
    FUN_000001 = "unc-1",
    FUN_000002 = "unc-2"
  )
)
```

Plot a genomic region with exon structures:

```r
ggexon(x) +
  geom_exon(
    chr = "RagTag_V",
    subset = c(21550000, 21680000)
  )
```

This gives you a compact exon-level view over a selected genomic interval while
keeping the plotting workflow close to ordinary `ggplot2`.

## Typical Workflow

A typical `ggexon` analysis looks like this:

1. Initialize a `SynIndividual`
2. Load the active feature annotation
3. Add extra layers such as variants, protein domains, or signal tracks
4. Patch annotations if curated models are available
5. Group individuals into a `SynSpecies` object when comparative context is
   needed
6. Plot exon structures or synteny-aware views from the same object model

## Working With Annotation Layers

One `SynIndividual` can carry multiple annotation types alongside the main
feature annotation.

```r
x <- add_annotation(
  x,
  SynVCFAnnotation(
    name = "variants",
    vcf_file = system.file(
      "extdata",
      "DL238.rename.ChrV.XZ1516.vcf.gz",
      package = "ggexon"
    )
  )
)

x <- add_annotation(
  x,
  SynProteinDomainAnnotation(
    name = "interpro",
    domain_file = system.file(
      "extdata",
      "InterProScan.tsv",
      package = "ggexon"
    ),
    keytype = "protein_id",
    source_db = "InterPro"
  )
)
```

Different annotation layers expose different query verbs:

```r
variant_layer <- get_annotation(x, "variants")
query_variants(variant_layer, chr = "V_RagTag", start = 21574336, end = 21574450)
```

This separation between genome container and annotation layers is one of the
main strengths of the package: feature annotation, variants, domains, and other
signals can be managed together without collapsing them into one table too
early.

## Comparative Genomics Workflow

For multi-genome analysis, store individuals and alignments inside a
`SynSpecies` object.

```r
sp <- SynSpecies(name = "Caenorhabditis")
sp <- add_individual(sp, x)

ggexon(sp) +
  geom_exon(
    chr = "RagTag_V",
    subset = c(21550000, 21680000)
  )
```

`SynSpecies` also supports pairwise and multiple-alignment objects, making it
possible to query alignment windows and build synteny-aware plots from the same
container.

## Learning More

The repository includes two vignettes that serve as the main walkthroughs:

- [vignettes/ggexon-workflow.Rmd](/Users/liudongyao/Downloads/repository/ggexon/vignettes/ggexon-workflow.Rmd)
- [vignettes/ggexon-classes-and-verbs.Rmd](/Users/liudongyao/Downloads/repository/ggexon/vignettes/ggexon-classes-and-verbs.Rmd)

They cover:

- the main object model
- annotation-layer management
- patching workflows
- protein translation
- species-level alignment handling

## Internals

For package internals and class relationships, see
[docs/object-model.md](/Users/liudongyao/Downloads/repository/ggexon/docs/object-model.md).

## Project Status

`ggexon` is under active development as a research-oriented package. The core
ideas and object model are already visible, but the interface should still be
treated as evolving while the package matures.

## Citation

If you use `ggexon` in academic work, cite the package repository and the
specific release or commit you used. If you plan to distribute the package
widely, adding a formal `CITATION` file would be a good next step.
