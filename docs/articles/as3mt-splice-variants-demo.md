# AS3MT: transcripts, protein start and DNA repeats

## Load the example

Compare human full-length AS3MT, a **paper-defined d2d3
reconstruction**, and chimpanzee full-length AS3MT. Coordinates use
**GRCh38** and **Pan_tro_3.0**, respectively. All inputs are bundled; no
downloads are needed.

``` r

library(ggexon)
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.4.3
demo_dir <- system.file("extdata", "as3mt", package = "ggexon")
source(file.path(demo_dir, "load-as3mt-example.R"), local = TRUE)
as3mt <- load_as3mt_example(demo_dir)
```

## 1. Transcripts and compressed introns

[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
shows CDS as thick boxes and UTR as thin boxes. Keep the full-reference
exon numbering: d2d3 retains **E1 and E4–E11**; dashed boxes mark
skipped E2/E3. `scale_x_ggexon_genomic(intron_factor = 30)` compresses
introns 30-fold while keeping exon widths.
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
automatically follows that same piecewise scale in both annotation
panels.

``` r

as3mt_plot <- plot_as3mt_example(as3mt, intron_factor = 30, show_protein = FALSE)
as3mt_plot$plot
```

![Human full-length and paper-defined d2d3 transcript rows above the
chimpanzee reference. Gold E2/E3 are skipped in d2d3; ribbons follow
30-fold intron
compression.](as3mt-splice-variants-demo_files/figure-html/as3mt-pairwise-1.png)

Transcript comparison; introns compressed 30-fold.

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

## 2. Protein change on the human reference

The assigned d2d3 protein contains **273 aa**, corresponding to
canonical human residues **103–375** instead of 1–375. Its loss of **102
N-terminal aa** is a consequence of the assigned downstream start; E2/E3
skipping removes 169 RNA bases, not a 306-nt in-frame deletion.

Project residue 103 through the **full-length human CDS** with
[`project_mutations_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_mutations_to_genome.md).
Its codon is **GRCh38 chr10:102872584–102872586** in E4. The example
helper returns that projection:

``` r

protein_change <- as3mt_protein_change(as3mt)
knitr::kable(protein_change[, c("position", "xmin", "xmax", "lost_aa")],
  col.names = c("Canonical residue", "Codon start", "Codon end", "Lost aa"))
```

| Canonical residue | Codon start | Codon end | Lost aa |
|------------------:|------------:|----------:|--------:|
|               103 |   102872584 | 102872586 |     102 |

Show the protein consequence with two distinct annotations:

- An orange interval marks canonical **aa1–102 absent from d2d3**. It is
  projected through the full-length CDS and split across coding exons;
  introns are not highlighted. This is a protein loss, not a 306-bp
  genomic deletion.
- A vertical start tick marks **M103**, which becomes **M1 in d2d3**.
  Both residues are methionine; there is no amino-acid substitution at
  this site.

``` r

protein_loss <- as3mt_protein_loss(as3mt)
as3mt_plot$plot + as3mt_protein_layers(protein_change, protein_loss)
```

The helpers use
[`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md)
for the absent protein interval, then
[`geom_rect()`](https://ggplot2.tidyverse.org/reference/geom_tile.html),
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
and
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
for its display. All layers stay on the human reference panel and follow
the genomic scale. Substitution lollipops are reserved for actual
reference/alternative residues.

## 3. DNA detail at the exon-1 repeat

The selected human reference contains three **36-bp VNTR** units; the
chimp reference has one imperfect homolog (34/36 matching bases). The
terminal unit contains 24 exonic and 12 intronic bases. Both references
retain the **GT** donor:

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

## Sources and reproducibility

- [Li et al. (2016),
  doi:10.1038/nm.4096](https://doi.org/10.1038/nm.4096): d2d3
  reconstruction, protein assignment and repeat sequence.
- [Cai et al. (2021),
  doi:10.1093/schbul/sbaa098](https://doi.org/10.1093/schbul/sbaa098):
  human VNTR minigene comparison.

The installed `extdata/as3mt` directory contains the plotting helper,
input snapshots and provenance. Repository preparation/export scripts
are in `data-raw/as3mt`; the genomic ribbons use LASTZ
`--nogapped --chain` blocks.

## Combined result

The human reference shows **aa1–102 absent** and a **start tick at M103
(d2d3 M1)**. Purple outlines locate the VNTR; each species keeps its own
DNA callout. Separate bars below the legend show exon and
compressed-intron scales. The lower human transcript remains a
**paper-defined reconstruction**; the repeat-count strip summarizes the
tested human alleles schematically.

``` r

grid::grid.newpage()
grid::grid.draw(sequence_example$grob)
```

![Combined AS3MT transcript comparison with an orange interval split
across human coding exons for canonical residues 1–102 absent from d2d3,
and a separate E4 start tick labelled M103 to M1. Purple VNTR units,
species-specific DNA callouts, and separate exon/intron scale bars
complete the
figure.](as3mt-splice-variants-demo_files/figure-html/as3mt-alignseq-1.png)

AS3MT transcripts, assigned protein-start change and exon-1 VNTR.
