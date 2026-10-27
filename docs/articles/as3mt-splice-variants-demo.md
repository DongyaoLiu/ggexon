# AS3MT: transcripts, DNA and RNA-seq

## Step 1. Load the example

Compare human full-length AS3MT, a **paper-defined d2d3
reconstruction**, and chimpanzee full-length AS3MT. Coordinates use
**GRCh38** and **Pan_tro_3.0**, respectively. All inputs are bundled; no
downloads are needed.

``` r
library(ggexon)
library(ggplot2)
demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
source(file.path(demo_dir, "load-as3mt-example.R"), local = TRUE)
as3mt <- load_as3mt_example(demo_dir)
```

## Step 2. Transcripts and compressed introns

[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
shows CDS as thick boxes and UTR as thin boxes. Keep the full-reference
exon numbering: d2d3 retains **E1 and E4–E11**; dashed boxes mark
skipped E2/E3. `scale_x_ggexon_genomic(intron_factor = 30)` compresses
introns 30-fold while keeping exon widths.
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
automatically follows that same piecewise scale in both annotation
panels. The build trains the maps from exon layers first, then applies
them to related genomic layers; adding coverage or labels does not
change the exon/intron boundaries.

``` r
as3mt_plot <- plot_as3mt_example(as3mt, intron_factor = 30)
as3mt_plot$plot
```

![Human full-length and paper-defined d2d3 transcript rows above the
chimpanzee reference. Gold E2/E3 are skipped in d2d3; ribbons follow
30-fold intron
compression.](as3mt-splice-variants-demo_files/figure-html/as3mt-pairwise-1.png)

The helper uses this grammar:

``` r
ggexon(as3mt$comparison) +
  geom_exon2(aes(xmin = xmin - 0.5, xmax = xmax + 0.5,
    ymin = isoform_y, transcripts = transcript_name, fill = exon_class),
    annotation_type = "all", compress_introns = FALSE,
    utr_height = 0.35, cds_height = 1) +
  geom_nuclink(alignment = "human_chimp", aes(fill = identity_band),
    inherit.aes = FALSE) +
  facet_genomics(vars(track), scales = "free_x", ncol = 1) +
  scale_x_ggexon_genomic(intron_factor = 30,
    guide = guide_x_ggexon_piecewise(by = "panel"))
```

Ribbons retain the two highest LASTZ identity categories, **97.1–98.3%**
and **98.3–100%**. The 37 ungapped blocks split into 57 polygon pieces
at scale boundaries. They show DNA similarity, not expression or proof
of exon skipping.

## Step 3. DNA detail at the exon-1 repeat

The repeat is a **variable number tandem repeat (VNTR)** of 36-bp units
at the exon 1 / intron 1 boundary. Each unit contains 24 exonic and 12
intronic bases, and the terminal unit retains the **GT** donor. The
selected human reference has three copies; the chimp reference has one
imperfect homolog (34/36 matching bases). The **−72 bp** difference is
therefore two missing 36-bp units (2 × 36 = 72 bp), not a 72-bp indel.
Both references retain the donor:

``` text
CCTGAGTCGCAGGCCGAGGAGACA | GTGAGTGCGCGC
         human exon 1  | intron 1
```

Prepare two aligned strings with an explicitly named human reference.
`-` denotes a gap and does not consume genomic bases. The strings are
verified against the bundled FASTAs; the donor-anchored alignment places
72 chimp gap columns opposite two human repeat copies.

``` r
v <- readRDS(file.path(demo_dir, "vntr-dna.rds"))
sequence_data <- alignseq_data(v$aligned, v$anchors, reference = "human",
  regions = v$regions, details = v$details, flank = 0)
```

[`geom_alignseq()`](https://dongyaoliu.github.io/ggexon/reference/geom_alignseq.md)
places each species’ DNA on its own panel. Default markers show
differences, with DNA callouts for selected details. Letters are equally
spaced schematically; smooth connectors retain genomic anchors. For a
short window, `mode = "rows"` shows all selected bases:

``` r
sequence_example <- plot_as3mt_alignseq(demo_dir, intron_factor = 30)
sequence_example$rows
```

![Human and chimp terminal repeat DNA on separate annotation panels. The
donor divides 24 exonic from 12 intronic
bases.](as3mt-splice-variants-demo_files/figure-html/as3mt-alignseq-rows-1.png)

Cai et al. tested otherwise matched **two- and three-repeat human
minigenes**; three repeats produced more d2d3. This supports
repeat-dependent splicing regulation, but does not establish sufficiency
for the isoform’s evolutionary origin. The human allele experiment
differs by **36 bp**; the figure’s **−72 bp** marker instead compares
the selected chimp and human references. Two base differences in the
chimp homolog are context, not demonstrated causal SNVs.

## Step 4. RNA-seq and splice junctions

The bundled GTEx frontal-cortex (BA9) sample
**GTEX-15G19-0011-R10b-SM-7KUG4.1** has both exon-2/3 inclusion and
E1→E4 skipping reads. Its [recount3 BigWig](https://rna.recount.bio/)
contains total coverage;
[Snaptron](https://snaptron.cs.jhu.edu/data.html) supplies junction
counts from the same sample. Both use **GRCh38**.
[`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md)
and
[`geom_junction()`](https://dongyaoliu.github.io/ggexon/reference/geom_junction.md)
follow the human exon/intron map automatically.

``` r
rna_example <- plot_as3mt_coverage(demo_dir, intron_factor = 30,
  junction_panel = "coverage")
rna_example$plot
```

![Matched GTEx coverage and counted blue inclusion/orange E1-to-E4
skipping arcs in the same panel, above the human AS3MT transcripts. All
follow 30-fold intron compression; ungapped links connect the chimp
reference.](as3mt-splice-variants-demo_files/figure-html/as3mt-rnaseq-1.png)

Arc labels are **junction read counts**: E1→E2, 9; E2→E3, 10; E3→E4, 11;
E1→E4, 12. Line width reflects these counts. The layer takes exon-edge
genomic anchors; it draws the curve and centres its label after
compression. Choose `panel = "coverage"` to put arcs on the BigWig
panel, or `panel = "annotation"` (the geom default) to put them on the
gene model:

``` r
geom_junction(data = rna_example$junctions,
  aes(x = x, xend = xend, y = y, yend = yend, height = height,
      linewidth = reads, label = reads, hjust = label_hjust, colour = event),
  panel = "coverage", inherit.aes = FALSE)
```

`y` and optional `yend` meet the measured coverage at the donor and
acceptor exon bases. `height` controls the curve’s rise; staggered
heights and text `hjust` separate the counts around narrow compressed
introns. `y`, `yend` and `height` use the selected panel’s y units. For
the gene-model layout, use
`plot_as3mt_alignseq(demo_dir, junction_panel = "annotation")`.

Source coverage values remain unchanged, and missing intervals are not
filled. `human-rnaseq-GTEX-15G19-provenance.json` records the exact
URLs, sample and extraction; `data-raw/as3mt/prepare-gtex-rnaseq.py`
reproduces the snapshots. These short-read junctions support the splice
events, not complete transcript abundances. This independent donor’s
VNTR genotype is not inferred from the reference DNA or the separate
minigene experiment.

## Step 5. Combined result

The green RNA-seq signal and counted splice arcs occupy the same panel
and share the human gene model’s compressed scale. Blue arcs support
exon inclusion; the orange arc supports E1→E4 skipping. Purple outlines
locate the VNTR; each species keeps its own DNA callout. Separate bars
below the legend show exon and compressed-intron scales. The lower human
transcript remains a **paper-defined reconstruction**; the repeat-count
strip summarizes the tested human alleles schematically.

``` r
grid::grid.newpage()
grid::grid.draw(sequence_example$grob)
```

![Matched human RNA-seq and counted inclusion/skipping arcs share the
top panel above the AS3MT transcript comparison. The same figure
includes ungapped links, purple VNTR units, species-specific DNA
callouts, exon/intron scale bars and the human minigene
comparison.](as3mt-splice-variants-demo_files/figure-html/as3mt-alignseq-1.png)

## Sources and reproducibility

- [Li et al. (2016),
  doi:10.1038/nm.4096](https://doi.org/10.1038/nm.4096): d2d3
  reconstruction and repeat sequence.
- [Cai et al. (2021),
  doi:10.1093/schbul/sbaa098](https://doi.org/10.1093/schbul/sbaa098):
  human VNTR minigene comparison.
- [recount3](https://rna.recount.bio/) and
  [Snaptron](https://snaptron.cs.jhu.edu/data.html): matched GTEx
  coverage and exon-junction reads.

The installed `extdata/as3mt` directory contains the plotting helper,
input snapshots and provenance. Repository preparation/export scripts
are in `data-raw/as3mt`; the genomic ribbons use LASTZ
`--nogapped --chain` blocks.
