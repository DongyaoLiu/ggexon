# Human AS3MTd2d3: Exon Skipping and Neural Phenotypes

## What this example compares

This example compares the selected human full-length **AS3MT**
transcript, a **paper-defined AS3MTd2d3 reconstruction**, and the
selected chimpanzee full-length reference. Full-reference exon numbers
are retained: the short human model contains **E1 and E4–E11**. Its
transcript ends are reconstructed from the current canonical reference;
no stable accession for that exact reconstruction was independently
established.

Human coordinates are from **GRCh38**, and chimpanzee coordinates from
**Pan_tro_3.0**, using the supplied Ensembl release-116 annotation
snapshot. The models retain their own assembly coordinates. They are
neither measured expression tracks nor a matched human–chimpanzee
phenotype experiment.

## Load one offline comparison

The bundled loader constructs a `SynSpecies` containing two
`SynIndividual` objects, their CDS/UTR annotations, a
`SynPairAlignment`, and stored genomic windows. No network access, LASTZ
installation, or working-directory paths are needed to render the
tutorial. These helpers are example-level code, not new exported package
functions.

``` r
library(ggexon)
library(ggplot2)
demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
source(file.path(demo_dir, "load-as3mt-example.R"), local = TRUE)
as3mt <- load_as3mt_example(demo_dir)
as3mt$comparison
#> An object of class "SynSpecies"
#>   name: AS3MT human-chimpanzee transcript comparison 
#>   individuals: 2 
#>   pairwise_alignments: 1 
#>   multiple_alignments: 0 
#>   homology_annotations: 0 
#>   locus_sets: 0 
#>   tree: FALSE 
#>   tree_plot: FALSE
```

The selected human references are `NM_020682.4 / ENST00000369880.8` and
`NP_065733.2`; the selected chimpanzee transcript is
`ENSPTRT00000007262.6`. Original annotation snapshots, sequence hashes,
the specification and source cells are preserved under the bundled
`extdata/as3mt` directory.

## One full-locus view with smaller introns

``` r
as3mt_plot <- plot_as3mt_example(as3mt, intron_factor = 30)
as3mt_plot$plot
```

![Human full-length and reconstructed AS3MTd2d3 transcript rows above a
chimpanzee full-length row. Gold E2 and E3 are skipped in the
reconstructed row. Ungapped DNA ribbons in the two highest identity
categories follow the same 30-fold intron-compressed piecewise scale as
the exon
annotations.](as3mt-splice-variants-demo_files/figure-html/as3mt-pairwise-1.png)

AS3MT transcripts with introns compressed 30-fold.

**Gold E2/E3** occur in the human full-length transcript. Dashed
outlines mark their skipped positions in d2d3. E1–E4 remain
distinguishable without a second zoom panel. Separate exon/intron scale
bars state their actual base-pair widths and the compression factor.

Thick boxes show CDS including the stop codon; thin boxes show UTR. Only
the two highest LASTZ identity categories are shown. The ungapped DNA
blocks contain no within-block indels and do not measure expression.
Links follow the exon/intron scale boundaries automatically.

The core grammar used inside the helper is:

``` r
ggexon(as3mt$comparison) +
  geom_exon2(
    aes(xmin = xmin - 0.5, xmax = xmax + 0.5,
        ymin = isoform_y, transcripts = transcript_name, fill = exon_class),
    annotation_type = "all", compress_introns = FALSE,
    utr_height = 0.35, cds_height = 1
  ) +
  geom_nuclink(alignment = "human_chimp", aes(fill = identity_band),
               inherit.aes = FALSE) +
  facet_genomics(vars(track), scales = "free_x", ncol = 1) +
  scale_x_ggexon_genomic(
    intron_factor = 30,
    guide = guide_x_ggexon_piecewise(by = "panel")
  )
```

The half-base box edges preserve inclusive exon lengths. Setting
`compress_introns = FALSE` on the geom leaves compression to the scale,
avoiding a second schematic transformation.

### Links follow the mosaic exon/intron scale

[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
automatically splits each link at the union of scale boundaries crossed
in **either** annotation panel. Each resulting piece uses its target
panel’s map at one end and its query panel’s map at the other. Changing
`intron_factor` therefore updates both the annotations and links. Raw
alignment coordinates remain genomic; no example-specific endpoint
conversion is used. Stored genomic windows are also mapped into display
limits automatically.

Within an ungapped alignment block this preserves nucleotide
correspondence. For general interval-only gapped links, subdivision
retains proportional endpoint correspondence; use ungapped blocks when
exact internal nucleotide correspondence is needed.

``` r
link_layer <- which(vapply(as3mt_plot$plot$layers,
  function(layer) inherits(layer$geom, "GeomPanel"), logical(1)))
vertices <- as3mt_plot$build$data[[link_layer]]
data.frame(
  original_blocks = length(unique(vertices$parent_group)),
  native_polygon_pieces = length(unique(vertices$group))
)
#>   original_blocks native_polygon_pieces
#> 1              37                    57
```

The **37 original blocks become 57 polygon pieces**. Built vertices
retain `genomic_x`, `parent_group` and `link_piece` for inspection.

### How the alignment was generated

Reference chromosome-10 windows were downloaded from Ensembl and
assembly checked: human `102869470–102901899` and chimpanzee
`104324156–104356197`. The recorded LASTZ 1.04.52 command is:

``` sh
lastz human.fa chimp.fa --strand=both --nogapped --chain \
  --format=paf:minimap2 --output=human_chimp.nogap.paf
```

Every one of the **124 raw blocks** was checked for a single gap-free
CIGAR, equal query/target/alignment spans, and a matching-base count
agreeing with the input DNA. Filtering for length ≥80 bp and identity
≥50% retained **93 blocks**. The plot keeps only the two highest
identity quantile categories: **97.1–98.3%** and **98.3–100.0%**
(rounded labels), totaling **37 blocks**. Those category boundaries were
calculated before this final selection and were not recalculated
afterward. Reverse-strand sequence matches can occur; links are not
themselves evidence for exon correspondence or exon skipping.

## Anchored DNA differences with `geom_alignseq()`

### Display the experimentally supported exon-1 VNTR

This example focuses on the **36-bp exon-1 tandem repeat (VNTR)**, which
has functional evidence for regulating AS3MTd2d3 splicing. It replaces
the arbitrary substitution and one-base indel formerly used to
demonstrate sequence drawing.

Li et al. (2016), Supplementary Fig. 3a, identifies the repeat sequence.
The supplied handoff maps three exact copies in the selected **GRCh38**
reference:

``` r
v <- readRDS(file.path(demo_dir, "vntr-dna.rds"))
v$motif
#> [1] "CCTGAGTCGCAGGCCGAGGAGACAGTGAGTGCGCGC"
knitr::kable(v$human_units, col.names = c("GRCh38 start", "GRCh38 end", "Repeat"))
```

| GRCh38 start | GRCh38 end | Repeat |
|-------------:|-----------:|-------:|
|    102869498 |  102869533 |      1 |
|    102869534 |  102869569 |      2 |
|    102869570 |  102869605 |      3 |

The unit is **CCTGAGTCGCAGGCCGAGGAGACAGTGAGTGCGCGC**. Each of the three
copies was checked base for base against the offline human genomic
FASTA. The last copy spans **102869570–102869605**: its first **24 bases
are in E1**, and its last **12 bases are in intron 1**. The E1 donor
lies between **102869593 and 102869594**, immediately before the
conserved **GT**:

``` text
CCTGAGTCGCAGGCCGAGGAGACA | GTGAGTGCGCGC
            exon 1      | intron 1
```

The selected **Pan_tro_3.0** reference has one imperfect homologous unit
at **104324211–104324246**, matching **34 of 36** human bases. Its donor
also lies after unit base 24 (between **104324234 and 104324235**), with
the same **GT**. This is a single homologous segment, rather than an
exact copy of the human motif. An exact-motif search returning zero in
chimp would not establish that the homologous sequence is absent.

### Functional evidence: two human repeat alleles

Cai et al. tested human **two-repeat and three-repeat minigenes**,
constructed to differ only at the VNTR. The three-repeat constructs
produced more d2d3 in the tested cell lines. This is the experimentally
supported regulatory variant shown in the figure: **repeat-number
variation**, rather than a demonstrated causal SNV at E2/E3. [Cai et
al., 2021](https://doi.org/10.1093/schbul/sbaa098).

The figure separates the human/chimp reference comparison from this
human allele experiment. The selected human reference contains three
repeats; it is not a claim that all humans have three. Published human
repeat counts include two, three and four. The evidence strip depicts
**motif-count reconstructions** of the tested two/three-repeat contrast,
not deposited full plasmid sequences or newly measured expression
values. Functional regulation does not by itself establish that this
expansion was sufficient for the evolutionary origin of the reported
human-specific isoform.

``` r
sequence_example <- plot_as3mt_alignseq(demo_dir, intron_factor = 30)
grid::grid.newpage()
grid::grid.draw(sequence_example$grob)
```

![Human AS3MT reference with three purple-outlined 36-base VNTR copies,
compared with the chimp single imperfect homolog. Each annotation panel
displays its own terminal repeat DNA, with the E1 donor marked after
base 24. Two external scale bars and a separate schematic summarize the
experimentally tested human two-repeat and three-repeat minigene
comparison.](as3mt-splice-variants-demo_files/figure-html/as3mt-alignseq-1.png)

Exon-1 VNTR and the human two-/three-repeat minigene comparison.

Purple outlines identify the repeat units in the genomic annotation. The
**−72** marker means chimp has 72 fewer aligned repeat bases relative to
this human reference. It does not assign ancestral insertion/deletion
direction, and it is not the experimentally tested **36-bp difference
between two and three human repeats**. The two base differences within
the chimp homolog are sequence context, not labelled as splicing-causal
SNVs.

Human DNA remains on the human annotation panel and chimp DNA on the
chimp panel. Smooth connectors retain genomic anchors while equally
spaced letters are schematic. The vertical boundary in each sequence row
marks the verified E1 donor without altering the DNA. Two scale bars sit
below the colour legend: **500 exon bp** and **15 intron kb at 30-fold
compression** have equal displayed lengths, calibrated to the common
annotation-panel scale.

### How the sequence layer is prepared

``` r
sequence_data <- alignseq_data(
  aligned = v$aligned, anchors = v$anchors, reference = "human",
  regions = v$regions, details = v$details, flank = 0
)
sequence_data$y <- ifelse(sequence_data$sequence == "human", 2.65, 1.5)
sequence_data$boundary_after <- !is.na(sequence_data$position) &
  sequence_data$position == v$donor_after[sequence_data$sequence]
sequence_data$boundary_label <- ifelse(sequence_data$boundary_after,
  "E1 donor | intron 1", "")
# Only repeat-copy difference markers are emphasized; base differences remain
# visible in the terminal-unit sequence as comparative context.
sequence_data$event_id[sequence_data$event_type == "substitution"] <- NA_integer_
as3mt_plot$plot + geom_alignseq(data = sequence_data)
```

[`geom_alignseq()`](https://dongyaoliu.github.io/ggexon/reference/geom_alignseq.md)
takes two prealigned strings, an explicit reference and one one-based
genomic anchor per sequence. Gaps (`-`) do not consume genomic bases.
Minus-strand strings must already be oriented for comparison; anchors
represent contiguous genomic DNA. Optional `boundary_after` and
`boundary_label` columns mark verified splice boundaries in the
callouts.

This VNTR uses an explicit **donor-anchored repeat alignment**: the
chimp unit aligns to the human terminal, donor-containing unit, with 72
gap columns opposite the other two human copies. Identical repeat copies
make equivalent gap placements possible; this alignment does not
identify the individual history of the repeated copies. Both ungapped
strings were verified against the offline assembly FASTAs. The original
longest gapped LASTZ block starts within the human array and is
therefore not used to infer the complete VNTR copy difference. It
remains bundled separately as `aligned-dna.rds` for audit; the
overview’s ungapped ribbons are unchanged.

### Optional row mode: the donor-containing unit

For a short window, `mode = "rows"` displays every selected base on its
own annotation panel. Here each row contains the 36-base terminal
unit/homolog. The grey exon segment covers 24 bases and the intron
backbone the remaining 12, using each assembly’s genomic coordinates.

``` r
sequence_example$rows
```

![Separate human and chimp annotation panels display their 36-base
terminal repeat or homolog. A donor boundary separates 24 exonic bases
from 12 intronic bases, and GT is conserved in both
references.](as3mt-splice-variants-demo_files/figure-html/as3mt-alignseq-rows-1.png)

``` r
short <- alignseq_data(v$aligned, v$anchors, reference = "human",
  regions = v$details)
geom_alignseq(data = short, mode = "rows")
```

The layer uses `GeomText` with optional nucleotide backgrounds. Selected
nearby detail windows merge when their flanking columns overlap. Omitted
columns split callouts; row mode requires one contiguous window. The
default maximum detail length is 60 columns; the VNTR terminal unit uses
36.

## Transcript, protein and organoid interpretation

![Three-panel figure showing schematic AS3MT exon skipping, full-length
protein residues 1–375 versus the assigned d2d3 protein at 103–375, and
human organoid SOX2 and TUJ1 reported percentages with mean and
SEM.](as3mt/AS3MT-exon-skipping-neural-phenotypes.png)

The overview’s exon slots are **schematic**, unlike the
assembly-specific, piecewise genomic scale in the comparison above.
Skipping E2/E3 removes **169 RNA nucleotides**, destroys the original
E1/E2-split start codon, and uses the handoff-assigned downstream start
in E4. The assigned **273-aa** protein corresponds to full-length
residues **103–375**, losing 102 N-terminal amino acids. This is not a
306-nt in-frame exon deletion. The reconstructed 2,281-nt RNA length is
conditional on the selected ends and reference repeat allele.

The organoid data compare control with AS3MTd2d3 overexpression in
H9/WA09 human forebrain organoids, electroporated at day 50. Each SOX2
or TUJ1 endpoint has **7 control and 11 overexpression reported
values**. Points show every supplied percentage; summaries use the
arithmetic mean and sample SD divided by √n. The publication **does not
define the sampling unit**: these counts must not be described as
independent donors or organoids. The reported two-sided unpaired-t-test
P values are **0.0008** for SOX2 and **0.0014** for TUJ1.

``` r
summary <- as3mt$organoid_summary
summary$group <- c("Control", "AS3MTd2d3 overexpression")[summary$group]
knitr::kable(summary, digits = 3)
```

| marker       | group                    |   mean |   sem |   n |
|:-------------|:-------------------------|-------:|------:|----:|
| SOX2+ / GFP+ | Control                  | 49.692 | 2.659 |   7 |
| SOX2+ / GFP+ | AS3MTd2d3 overexpression | 37.578 | 1.645 |  11 |
| TUJ1+ / GFP+ | Control                  | 45.184 | 2.072 |   7 |
| TUJ1+ / GFP+ | AS3MTd2d3 overexpression | 60.101 | 2.768 |  11 |

Li et al. did not detect the chimp E1–E4 junction in the examined
pooled-tissue RNA-seq, which included brain. This is not proof of
absence in every tissue. Healthy humans also express the short isoform;
the experiment does not measure species-specific disease rates.

## Sources and reproducibility

- Li et al. (2016), *Nature Medicine*,
  [doi:10.1038/nm.4096](https://doi.org/10.1038/nm.4096).
- Cai et al. (2021), *Schizophrenia Bulletin*,
  [doi:10.1093/schbul/sbaa098](https://doi.org/10.1093/schbul/sbaa098),
  VNTR minigene experiments on AS3MTd2d3 splicing.
- Kim et al. (2025), *Science Advances*,
  [doi:10.1126/sciadv.adp8271](https://doi.org/10.1126/sciadv.adp8271),
  Supplementary Fig. S8B–C and Table S5, cells A34:A40 and B34:B44.

The repository’s `data-raw/as3mt` directory contains the
alignment-preparation and figure-export scripts. The loader and all
plotting inputs are bundled in the installed package under
`extdata/as3mt`. The optional rat-neuron and spindle-angle panels are
not included in these figures.
