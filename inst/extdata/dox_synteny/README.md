# Compact three-species Dox synteny example

Three species groups, two nucleotide-link panels, and 2/3/2 displayed loci.
Each species has one combined RNA subpanel above its annotation. The Dsim signal is
remapped from the complete original read library to the plotted assembly.
The tutorial prepares the native data objects, attached annotations,
alignments, and layouts first, then builds annotation-only,
annotation/linkage, and annotation/linkage/RNA figures layer by layer.
Detailed provenance and interpretation are documented below.
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
heights have ratio `1 : 0.5 : 0.5`, controlled by `facet_genomics()`'s
`link_panel_height` and `coverage_panel_height` arguments. `scales = "free_x"`
keeps the genomic axes independent. `scale_panel_coverage("fixed_y")` shares
one RNA y scale, with limits from zero through 1.1 times the largest peak
across species. Each track sums the magnitudes of forward and reverse scores
at each genomic base, without smoothing or additional normalization.
The legend marks the signal as ×1; no local region is magnified.
Dsim read depth and the deposited outer-species signal have different units,
so peak heights do not measure
relative expression across species.

`center_panel_annotation()` centres gene bodies without manual annotation
y limits. RNA y axes are on the right. The outside species labels use
`facet_genomics(strip.position = "left", strip_group = "individual")` with
`theme_ggexon_side_strips("left")`, so one name spans each combined RNA/gene block.
Both RNA and gene panels end at 9.44 Mb for Dmau and Dsech; Dsim keeps its
source window. This display override does not change `windows.tsv` or the data.

Files:

- `objects/`: three GTFs, two PAFs, and nine BigWigs for the object-first
  tutorial. GTFs contain gene/transcript spans and disjoint CDS/UTR pieces;
  PAFs encode the verified LASTZ blocks. Three `.RNA.bw` files sum forward
  and reverse signal magnitudes at each base. The six original `.RNAplus.bw`
  and `.RNAminus.bw` files remain available for Basic usage.
  `objects/manifest.json`
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
  defines three coding exons in `features.tsv`, at X:9,250,280-9,250,879 on
  the minus strand (CIGAR `285M63N42M63N147M`). UTRs and the full-length
  transcript are not reconstructed.
- `coverage.tsv.gz`: deposited Dmau/Dsech forward/reverse BigWig scores, plus
  Dsim per-base primary read depth from remapped SRR23418969. All rows use the
  exact plotted assembly coordinates. Negative reverse scores are reflected
  to positive magnitudes before combining the two strands for plotting.
  No smoothing or additional library normalization is applied.
  Dsim excludes secondary/supplementary alignments,
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

## Data provenance and interpretation

The flanks are common *D. melanogaster* RefSeq models `NM_132346` and
`NM_132348` mapped by UCSC GenArk to the selected species assemblies. The
orange model comes from the published 474-nt Dox CDS. A unique, end-to-end,
100%-identity splice alignment places it at Dsim X:9,250,280-9,250,879 on the
minus strand, with CIGAR `285M63N42M63N147M`. Joining those three coding exons
and reverse-complementing them reconstructs the published CDS exactly.
The figure does not reconstruct the full Dox transcript.

RNA-seq comes from [GSE185361](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE185361),
specifically [GSM5611558](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM5611558),
[GSM7039340](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM7039340), and
[GSM5611560](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM5611560).
The deposited Dsim BigWigs use an X chromosome of 22,976,950 bp, whereas the
plotted GenBank assembly has 23,123,156 bp. They were excluded. The complete
original library [SRR23418969](https://www.ebi.ac.uk/ena/browser/view/SRR23418969),
containing 40,381,684 read pairs, was remapped directly to GCA_004382185.1
with minimap2's `splice:sr` preset. No coordinate offset was inferred.

Dsim coverage retains primary alignments with MAPQ >=20 and excludes intron
and deletion bases; overlapping mates each contribute to read depth.
Canonical splice motifs support a reverse-stranded paired library, with read 1
antisense and read 2 sense. Input checksums, the full reference sequence, the
BAM chromosome dictionary, and the complete-library read count were verified.
The coverage agrees with an independent samtools depth calculation at all
43,781 plotted bases. `Dsim-remapping-qc.json` records these checks and the
mapping statistics. Dmau and Dsech retain author-deposited coverage values.

`manifest.json` records original source URLs, sequence and data checksums,
BigWig extraction ranges, and alignment settings. `objects/manifest.json`
records how the native tutorial files relate to the verified tables, with
input/output checksums. The GTF/PAF conversion retains the source intervals.
The combined BigWigs sum forward and reverse signal magnitudes at each base;
the original strand files remain available for the Basic usage example.

For source details, see the [primary locus paper](https://doi.org/10.1038/s41559-021-01592-z),
its [Supplementary Table 1](https://media.springernature.com/original/springer-static/esm/art%3A10.1038%2Fs41559-021-01592-z/MediaObjects/41559_2021_1592_MOESM2_ESM.xlsx),
the [assembly paper](https://doi.org/10.1101/gr.263442.120), and the
[minimap2 short RNA-seq documentation](https://github.com/lh3/minimap2#map-short-rna-seq-reads).

To prepare the files again from the repository, run
`remap-dsim-rnaseq.py`, `build-dox-data.py`, then
`Rscript data-raw/dox_synteny/prepare-dox-objects.R`.
The large raw-read/reference/BAM cache stays outside the bundled inputs.
Rendering the tutorial uses only the bundled files.

See `vignette("dox-synteny-demo", package = "ggexon")` for the plot grammar.
Preparation scripts are in `data-raw/dox_synteny/`.
