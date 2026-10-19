# ggexon

**Grammar of genomic annotations, synteny, and associated data.**

[Documentation and tutorials](https://dongyaoliu.github.io/ggexon/)

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

## Flagship HOX Cluster Expansion Example

![A seven-row by four-column HOX matrix with cluster-specific human gene
symbols on the x axes, square gene boxes, the ancestral amphioxus
cluster displayed under HOXA, grey not-applicable cells, and a marked
zebrafish HOXDB cluster-not-retained
cell](reference/figures/hox-cluster-expansion-demo.png)

A seven-row by four-column HOX matrix with cluster-specific human gene
symbols on the x axes, square gene boxes, the ancestral amphioxus
cluster displayed under HOXA, grey not-applicable cells, and a marked
zebrafish HOXDB cluster-not-retained cell

The bundled HOX cluster-expansion tutorial compares six chordate species
in a seven-row by four-column matrix. HOXA through HOXD show the four
vertebrate cluster families and the duplicated zebrafish `a` and `b`
rows. The single ancestral amphioxus cluster is displayed under HOXA
only as a compact layout anchor; this placement does not assign it
specifically to HOXA. Its HOXB–HOXD cells are structural blanks. A
not-retained zebrafish HOXDB cluster and unoccupied gene slots within
retained clusters remain visually distinct.

Genes are drawn with
[`geom_genebox()`](https://dongyaoliu.github.io/ggexon/reference/geom_genebox.md)
as data-unit squares containing strand-direction arrows. Its default
`box_size = 1` spans one x and y data unit; `coord_fixed(ratio = 1)`
keeps the boxes visually square when the figure is rendered or exported.
`strip_scale_x(slot_order = ...)` aligns every gene to a shared
Hox15-to-Hox1 template, so genes assigned to the same curated Hox-number
slot occupy exactly the same x coordinate. Box position represents that
paralog-slot alignment, whereas the arrow independently represents
transcription direction; amphioxus therefore retains its left-pointing
arrows without breaking homolog-slot alignment. Unoccupied positions
remain visible and do not by themselves imply gene loss.
[`scale_x_facet_labels()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_facet_labels.md)
supplies cluster-specific human reference gene symbols from an explicit
named list, with `HoxN` fallback labels for slots absent from the human
reference cluster. All facets retain the same fixed x-coordinate range
and break positions, so changing the labels preserves slot alignment and
works with `coord_fixed(ratio = 1)`. Labels are rotated 90 degrees to
fit the 15 slots in each narrow column. The example uses
[`theme_ggexon_clean()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_clean.md)
to remove panel outlines and facet-strip backgrounds. The tutorial
removes nucleotide and synteny links, and also demonstrates initiation,
genomic coding-midpoint, and stop anchor modes on the human HOXA
cluster, with any terminal-CDS positional proxies flagged in the bundled
data. Its annotations are pinned to Ensembl release 116 for vertebrates
and Ensembl Metazoa release 63 for amphioxus.

Open
[`vignette("hox-cluster-expansion-demo", package = "ggexon")`](https://dongyaoliu.github.io/ggexon/articles/hox-cluster-expansion-demo.md)
for the full example, selected-transcript provenance, and source audit
trail.

## HLA-A Alleles on an Exon–Intron Gene Model

![Dense amino-acid variant lollipops projected onto the HLA-A reference
gene, with eight coding exons and seven introns compressed
three-fold](reference/figures/hla-a-genomic-lollipop-demo.png)

Dense amino-acid variant lollipops projected onto the HLA-A reference
gene, with eight coding exons and seven introns compressed three-fold

The HLA-A tutorial summarizes 5,998 eligible named alleles as 1,212
amino-acid substitutions. It shows both a protein-domain view and a
splice-aware projection onto the exact A\*02:01:01:01 reference gene.
Exons retain their native scale;
`scale_x_ggexon_genomic(intron_factor = 3)` compresses introns
three-fold, with separate exon and intron scale bars. Counts describe
database representation, not population frequencies.

See the [HLA-A protein and gene-model
tutorial](https://dongyaoliu.github.io/ggexon/articles/hla-a-lollipop-demo.html).

## AS3MT Exon Skipping with Intron-Compressed Alignment Links

![Human and chimpanzee AS3MT transcripts with native exon widths,
30-fold intron compression, and ungapped genomic alignment links
following the same piecewise
scale](reference/figures/as3mt-splice-variants-demo.png)

Human and chimpanzee AS3MT transcripts with native exon widths, 30-fold
intron compression, and ungapped genomic alignment links following the
same piecewise scale

The AS3MT tutorial compares the human and chimpanzee full-length
transcripts with a paper-defined human AS3MTd2d3 reconstruction.
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
distinguishes CDS from UTR, and
`scale_x_ggexon_genomic(intron_factor = 30)` compresses introns while
retaining exon widths.
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
automatically subdivides raw genomic alignment blocks at both panels’
scale boundaries and maps the pieces to the same exon/intron scales. The
plot retains only the two highest identity categories of the ungapped
LASTZ alignment.

See the [AS3MT transcript and neural-phenotype
tutorial](https://dongyaoliu.github.io/ggexon/articles/as3mt-splice-variants-demo.html)
for the reproducible offline example, protein mapping, reported organoid
values, and reconstruction and sampling-unit caveats.

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

| Class                          | Role                                                                                                                              |
|--------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| `SynSpecies`                   | Project-level container for individuals, alignments, trees, and layout state                                                      |
| `SynIndividual`                | One genome, strain, species, or sample                                                                                            |
| `SynFeatureAnnotation`         | Genes, transcripts, exons, CDS, labels, and annotation patches                                                                    |
| `SynVCFAnnotation`             | VCF querying is supported. BCF parsing is not implemented; BCF input raises an unsupported-format error.                          |
| `SynBigWigAnnotation`          | Signal tracks queried by genomic interval                                                                                         |
| `SynProteinDomainAnnotation`   | Protein-domain intervals from InterProScan-like tables                                                                            |
| `SynProteinMutationAnnotation` | Protein mutation summaries for lollipop-style tracks                                                                              |
| `HomologyAnnotation`           | Cross-species gene homology mappings from BLAST results                                                                           |
| `SynPairAlignment`             | Pairwise genome links from PAF, PSL, or ODGI-derived data                                                                         |
| `SynMultiAlignment`            | Stores already parsed MAF data supplied through `data`; loading a MAF file remains unavailable. ODGI-backed loading is available. |
| `SynLayout`                    | Stored panel windows and layout decisions                                                                                         |

### Part 2: Functions Manipulate and Draw Classes

The public API is organized around a few function families:

| Task                         | Main functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
|------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Build object graphs          | [`add_individual()`](https://dongyaoliu.github.io/ggexon/reference/add_individual.md), [`add_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_annotation.md), [`add_homology_annotation()`](https://dongyaoliu.github.io/ggexon/reference/add_homology_annotation.md), [`add_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/add_pairwise_alignment.md), [`add_multiple_alignment()`](https://dongyaoliu.github.io/ggexon/reference/add_multiple_alignment.md), [`add_tree()`](https://dongyaoliu.github.io/ggexon/reference/add_tree.md)                                                                                                                                                                                                                                             |
| Load and query data          | [`load_annotation()`](https://dongyaoliu.github.io/ggexon/reference/load_annotation.md), [`load_alignment()`](https://dongyaoliu.github.io/ggexon/reference/load_alignment.md), [`import_blast_homology()`](https://dongyaoliu.github.io/ggexon/reference/import_blast_homology.md), [`query_features()`](https://dongyaoliu.github.io/ggexon/reference/query_features.md), [`query_variants()`](https://dongyaoliu.github.io/ggexon/reference/query_variants.md), [`query_signal()`](https://dongyaoliu.github.io/ggexon/reference/query_signal.md), [`query_domains()`](https://dongyaoliu.github.io/ggexon/reference/query_domains.md)                                                                                                                                                                                  |
| Derive sequence/protein data | [`extract_cds_seq()`](https://dongyaoliu.github.io/ggexon/reference/extract_cds_seq.md), [`translate_protein()`](https://dongyaoliu.github.io/ggexon/reference/translate_protein.md), [`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| Subset and curate objects    | [`subset_species()`](https://dongyaoliu.github.io/ggexon/reference/subset_species.md), [`subset_individual()`](https://dongyaoliu.github.io/ggexon/reference/subset_individual.md), [`subset_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/subset_pairwise_alignment.md), [`set_gene_labels()`](https://dongyaoliu.github.io/ggexon/reference/set_gene_labels.md), [`patch_annotation_from_gff()`](https://dongyaoliu.github.io/ggexon/reference/patch_annotation_from_gff.md)                                                                                                                                                                                                                                                                                                                      |
| Draw genomic layers          | [`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md), [`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md), [`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md), [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md), [`geom_genebox()`](https://dongyaoliu.github.io/ggexon/reference/geom_genebox.md), [`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md), [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md), [`geom_synteny_link()`](https://dongyaoliu.github.io/ggexon/reference/geom_synteny_link.md), [`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md)                                  |
| Arrange panels and guides    | [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md), [`scale_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_annotation.md), [`scale_panel_coverage()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_coverage.md), [`center_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/center_panel_annotation.md), [`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md), [`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md), [`scale_x_facet_labels()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_facet_labels.md), [`guide_x_ggexon_piecewise()`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md) |
| Style plots                  | [`theme_ggexon_clean()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_clean.md), [`theme_ggexon_base()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_base.md), [`theme_ggexon_track()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_track.md), [`theme_ggexon_pairwise()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_pairwise.md)                                                                                                                                                                                                                                                                                                                                                                                                                             |

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
- [`vignette("cd44-splice-variants-demo", package = "ggexon")`](https://dongyaoliu.github.io/ggexon/articles/cd44-splice-variants-demo.md)
  for human and mouse splice variants, LASTZ links, sequence windows,
  and supporting tables stored together in one `SynSpecies` object.
- [`vignette("hox-cluster-expansion-demo", package = "ggexon")`](https://dongyaoliu.github.io/ggexon/articles/hox-cluster-expansion-demo.md)
  for the Ensembl-pinned HOX cluster matrix, exact curated Hox-number
  slots, and
  [`geom_genebox()`](https://dongyaoliu.github.io/ggexon/reference/geom_genebox.md)
  anchor modes.
- [`vignette("bigwig-coverage-demo", package = "ggexon")`](https://dongyaoliu.github.io/ggexon/articles/bigwig-coverage-demo.md)
  for thirteen independent raw-depth BigWig panels with automatic
  span-weighted median-at-85%-height scales above one shared, centered
  gene annotation.
- [`?ggexon`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md)
  and the reference index for function-level documentation.

If you use `ggexon` in work, please also cite the preprint:

> Liu D, Zheng C. sept-1/zina-1 is an Ancient Toxin-Antidote System in
> Caenorhabditis elegans. bioRxiv. 2025.
> <https://www.biorxiv.org/content/10.1101/2025.11.28.691152v1>
