# Compact three-species Dox synteny example

Three species groups, two nucleotide-link panels, and 2/3/2 displayed loci.
Each species has an RNA subpanel above its annotation. The Dsim signal is
remapped from the complete original read library to the plotted assembly.
The tutorial constructs native data objects and builds annotation-only,
annotation/linkage, and annotation/linkage/RNA figures layer by layer.
Model descriptions, signal units, and provenance are in the tutorial text.
The source study is [Vedanayagam et al. (2021), Extended Data Figure 5](https://pmc.ncbi.nlm.nih.gov/articles/PMC8665063/#F11).
Local testis RNA-seq for all three species is from GSE185361.

From an installed ggexon package:

```r
library(ggexon)
demo_dir <- system.file("extdata", "dox_synteny", package = "ggexon")
source(file.path(demo_dir, "load-dox-example.R"))
example <- load_dox_example(demo_dir)
plot_dox_example(example)$plot
plot_dox_example(example, intron_factor = 3)$plot
plot_dox_example(example, coverage_panel_height = 0.5, link_panel_height = 0.5)$plot
```

The helper uses an explicit eight-row SynLayout. Annotation, link, and RNA
heights have ratio 1 : 0.5 : 0.5, controlled by `facet_genomics()`'s
`link_panel_height` and `coverage_panel_height` arguments. RNA y limits use
`scale_panel_coverage("free_y", limits = ...)`: zero through 1.1 times each
species' peak. Deposited outer-species scores and remapped Dsim read depths
are unchanged; visual peak heights are comparable because all RNA panels
use independent y scales. The units are explicitly different.

Files:

- `objects/`: three GTFs, two PAFs, and six BigWigs for the object-first
  tutorial. GTFs contain gene/transcript spans and disjoint CDS/UTR pieces;
  PAFs encode the verified LASTZ blocks; BigWigs retain plus and reverse
  magnitudes at the original genomic coordinates. `objects/manifest.json`
  records original table hashes and native file hashes. Prepare these views
  with `data-raw/dox_synteny/prepare-dox-objects.R` on macOS or Linux.
- `windows.tsv`: exact assembly accessions, original contigs, 1-based inclusive
  windows, species order, and GEO samples. Other tables use `X` as a seqname
  alias; they retain chromosome coordinates, not coordinates relative to FASTA.
- `Dmau.fa`, `Dsim.fa`, `Dsech.fa`: real sequence of each complete window.
- `features.tsv`: mapped RefSeq exon and CDS intervals for the two flanking
  genes. GenArk maps the D. melanogaster transcripts NM_132346 and NM_132348
  onto the indicated assemblies. They are comparative transcript models.
- `genes.tsv`: the two flank models and the additional Dsim Dox locus.
- `Dox-published-CDS.fa`: the 474-nt spliced CDS from Supplementary Table 1.
  A unique, end-to-end, 100%-identity splice alignment to the Dsim window
  defines three coding exons in `features.tsv`, at X:9,250,280–9,250,879 on
  the minus strand (CIGAR `285M63N42M63N147M`). UTRs and the full-length
  transcript are not reconstructed.
- `coverage.tsv.gz`: deposited Dmau/Dsech forward/reverse BigWig scores, plus
  Dsim per-base primary read depth from remapped SRR23418969. All rows use the
  exact plotted assembly coordinates. Negative reverse scores are reflected
  to positive magnitudes only for plotting. No smoothing or additional library
  normalization is applied. Dsim excludes secondary/supplementary alignments,
  MAPQ <20, intron and deletion bases; overlapping mates each count. Nonzero
  runs encode actual per-base depth without expanding the bundled file.
- `Dsim-remapping-qc.json`: complete-library read count, source FASTQ/reference
  MD5 checksums, genome SHA256, mapper/version/program headers, library-strand
  checks, mapping statistics, and independent per-base depth verification.
  The original incompatible BigWigs (X = 22,976,950 bp) remain excluded; the
  remapping uses GenBank X = 23,123,156 bp directly.
- `links.tsv`: actual ungapped, chained LASTZ blocks, length >=80 bp and
  identity >=85%; no gene-dependent filtering.
- `manifest.json`: source URLs, extraction byte-range checksums and settings,
  and MD5 checksums of the bundled sequence/data files.

See `vignette("dox-synteny-demo", package = "ggexon")` for the plot grammar
and source details. Preparation scripts are in `data-raw/dox_synteny/`.
