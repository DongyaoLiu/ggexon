# Haplotype representation of the MHC region across apes

This tutorial first renders the complete MHC data object, then subsets
it for a concise comparison. The remaining steps explain data
composition, gene and lollipop layers, nucleotide links and the facet
layout. The view recombines the gene identity and status information of
Extended Data Fig. 3a/b with the continuous region and alignment design
of Fig. 3c in [Yoo et
al. (2025)](https://www.nature.com/articles/s41586-025-08816-3). It is a
source-backed reconstruction, not a pixel reproduction of the published
figure.

## Full MHC comparison

The complete source dataset contains 13 haplotype tracks: human CHM13
and both haplotypes of six ape species. Build and render the complete
data object first. Run the code from a repository checkout root; these
helpers are repository example functions, not exported package APIs.

``` r

library(ggexon)
source("data-raw/ape_paper/mhc_tutorial_data.R")

mhc_all <- read_mhc_data()
stopifnot(length(individual_names(mhc_all$species)) == 13L,
          nrow(mhc_all$mhc) == 285L)
```

`mhc_all` contains the `SynSpecies` object (`species`), native windows,
MHC genes, context models, exons, variable regions and direct alignment
tables. `max_bp` records the longest window in the complete dataset. The
alignment collection contains the original 12 adjacent comparisons plus
two direct bridges used by the later subset example; the full view draws
only its 12 adjacent comparisons.

``` r

full_plot <- render_mhc_data(
  mhc_all,
  output_dir = "data-raw/ape_paper/results/mhc_full_example/figures"
)
full_plot$pdf  # exported vector figure
```

The full view below contains 285 curated MHC heads, the native-scale
gene arrows and grey context genes, lower annotation lanes, and 624
nucleotide ribbons. Each track retains its native distances and assembly
coordinate labels.

![Full thirteen-track MHC comparison. From top to bottom: both
haplotypes of siamang, Sumatran orangutan, Bornean orangutan, gorilla,
bonobo and chimpanzee, followed by human CHM13. Native gene spans,
enlarged MHC heads on shared rows, grey context genes, variable-region
annotations and adjacent nucleotide ribbons are
retained.](figures/mhc-continuous.png)

## Subset the full data object

For a shorter example, retain human CHM13, both gorilla haplotypes and
siamang h1. Gorilla’s two curated head profiles differ; siamang’s
profiles match in gene identity, copy count, order, strand and status,
so only one is shown. Human has only the CHM13 reference in these
inputs.

``` r

keep <- c("SSYh1", "GGOh2", "GGOh1", "HSAh1")  # top to bottom
mhc_small <- subset_mhc_data(mhc_all, tracks = keep)

stopifnot(
  identical(individual_names(mhc_small$species), keep),
  length(pairwise_alignments(mhc_small$species)) == 3L,
  nrow(mhc_small$mhc) == 97L,
  identical(mhc_small$max_bp, mhc_all$max_bp),
  length(individual_names(mhc_all$species)) == 13L  # original unchanged
)
```

The subset retains the selected individuals with their attached
annotations, filters every annotation table, keeps the direct alignments
for adjacent selected tracks and rebuilds the chain layout. It preserves
the full object’s `max_bp` and leaves `mhc_all` unchanged. The siamang
h1 → gorilla h2 and gorilla h1 → human bridges were computed directly
from the cached DNA, rather than inferred through removed tracks. A
requested pair without a stored direct alignment raises an error.

``` r

compact_plot <- render_mhc_data(
  mhc_small,
  output_dir = "data-raw/ape_paper/results/mhc_compact/figures"
)
compact_plot$pdf
```

The concise view below has four annotation tracks, 97 MHC heads, 860
grey context arrows and 252 ribbons. It keeps the small native MHC
arrows, six shared head rows, ortholog colors, pseudogene markings and
lower annotation lanes. At 180 × 267.5 mm it is about 67% shorter than
the full comparison.

![Concise four-track MHC comparison: siamang h1, gorilla h2, gorilla h1
and human CHM13. The subset preserves native gene spans, shared enlarged
head rows, grey context and lower annotations, with three direct
nucleotide-link panels.](figures/mhc-three-species.png)

Both examples use the same renderer. The following steps explain its
data composition and layers. Full-dataset counts in these steps refer to
the 13-track view. The website embeds verified previews because the
source tables under `data-raw/` are not installed with the package.
Helper definitions are in `data-raw/ape_paper/mhc_tutorial_data.R`.

## Step 1. What is in the MHC data?

All inputs are tab-separated tables under
`data-raw/ape_paper/results/mhc_continuous/`. The main files are:

| File | One row per | Key columns |
|----|----|----|
| `windows.tsv` | haplotype window | `track`, `label`, `chr`, `start0`, `end0`, `length`, `orientation` |
| `gene_models_with_recovered_context/models.tsv` | representative transcript | `track`, `model_id`, `gene_symbol`, `chromStart`, `chromEnd`, `strand`, `annotation_evidence`, `projection_coverage`, `projection_identity` |
| `gene_models_with_recovered_context/exons.tsv` | supporting exon | `model_id`, `start0`, `end0` |
| `mhc_full/genes.tsv` | curated MHC gene | `track`, `locus`, `start`, `end`, `strand`, `gene_name`, `functionality` |
| `links.tsv` | alignment block | `tspecies`, `qspecies`, `tstart`, `tend`, `qstart`, `qend`, `strand`, `identity` |
| `discordant_regions.tsv` | variable-region piece | `track`, `region`, `start0`, `end0`, `display_start0`, `display_end0` |
| `source_metadata/mhc_continuous/colors.txt` | ortholog key | gene key, RGB |

Coordinate conventions matter here: `windows.tsv` and the derived tables
use BED0, half-open coordinates (`start0`, `end0`). The curated
`genes.tsv` table stores one-based closed coordinates (`start`, `end`),
so the renderer converts `start` to `start0 = start - 1` before
plotting.

Representative rows look like this:

``` text
windows.tsv
track   label          chr             start0     end0       length    orientation
SSYh1   Siamang h1     chr23_hap1      37530567   42365376   4834809   -
PTRh1   Chimpanzee h1  chr5_hap1_hsa6  34745920   40035224   5289304   +
HSAh1   Human CHM13    chr6            28669878   33979214   5309336   +

genes.tsv
track   locus   start      end        strand  gene_name  functionality
HSAh1   MHC_I   29595529   29610626   +       HLA-F      functional

links.tsv
tspecies  qspecies  tstart  tend     qstart  qend     identity
SSYh2     SSYh1     1       113549   1       113407   97.9
```

The curated table has **285** records. `functionality` is the source
call (`functional` or `pseudogene`); `locus` separates class I from
class II; and `gene_name` supplies the identity key used for colors and
for the shared head rows. The context tables contain released models and
explicitly labelled same-species Liftoff projections, with
`annotation_evidence` distinguishing the two.

``` r

B <- "data-raw/ape_paper/results/mhc_continuous"

windows <- read.delim(file.path(B, "windows.tsv"))
links   <- read.delim(file.path(B, "links.tsv"))
regions <- read.delim(file.path(B, "discordant_regions.tsv"))
models  <- read.delim(file.path(B, "gene_models_with_recovered_context/models.tsv"))
exons   <- read.delim(file.path(B, "gene_models_with_recovered_context/exons.tsv"))
mhc     <- read.delim("data-raw/ape_paper/results/mhc_full/genes.tsv")
```

## Step 2. Data composition

Each track already has its own panel.
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
uses the panel’s x scale to position its data, and
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
uses the corresponding source panel scales when placing ribbon
endpoints. Panel limits handle fitting and zooming. **A manual
coordinate conversion is not required merely to display separate genomic
panels.**

This example’s preparation step brings its particular inputs into one
consistent coordinate system:

| Input | Stored coordinates | Composition for this example |
|----|----|----|
| Curated MHC gene spans | One-based closed native assembly coordinates | Convert to BED0, then express relative to the extracted window |
| Context models and VR intervals | BED0 native assembly coordinates | Express relative to the same window |
| Minimap2 alignment blocks | Coordinates relative to extracted sequences | Already window-relative; use the same convention as the gene layers |
| Siamang records | Native assembly orientation is reversed | Reflect gene and annotation coordinates to match the reverse-complemented alignment sequence |

The gene tables and alignment tables must agree about what a position
means. For this dataset, the renderer uses window-relative display
coordinates. On a plus-oriented track it subtracts the window start; on
a reverse-oriented track it reflects around the window end. These
operations change the origin and orientation, **not the genomic
distances**. Each row’s axis is labelled with its original native
assembly coordinates at export.

The helper below shows that data preparation in the renderer. It is not
an additional ggexon panel-fitting requirement:

``` r

transform_native <- function(d, a, b, trackcol = "track") {
  z  <- windows[match(d[[trackcol]], windows$track), ]
  lo <- pmax(d[[a]], z$start0)
  hi <- pmin(d[[b]], z$end0)
  d$xmin <- ifelse(z$orientation == "+", lo - z$start0, z$end0 - hi)
  d$xmax <- ifelse(z$orientation == "+", hi - z$start0, z$end0 - lo)
  d
}
```

**Independent fitting and a common bp scale are different choices.** If
each panel fills the same width with its own complete region, a 4.83-Mb
window and a 5.31-Mb window have different bp-per-mm ratios. That is a
valid zoomed view. Here we keep one bp scale across haplotypes so
expansions and deletions retain their relative sizes. The renderer
therefore gives every panel the same display span, taken from the full
object’s longest window:

``` r

max_bp <- mhc_all$max_bp
panel_xlim <- setNames(rep(list(c(0, max_bp)), nrow(windows)), windows$track)
panel_chr <- setNames(rep("MHC", nrow(windows)), windows$track)
```

The subset keeps this full-dataset span; it does not refit each retained
region to occupy the full width. Translation/reflection composes the
input coordinates; `xlim` and the panel scales control the view. Native
coordinates could also be used directly if gene, annotation and link
tables were all expressed in those same native coordinates.

The `SynSpecies` part is already assembled by `read_mhc_data()`. For the
full view, retaining all track IDs selects its original 12 adjacent
direct alignments and prepares the chain layout:

``` r

full_data <- subset_mhc_data(mhc_all, tracks = mhc_all$windows$track)
sp <- full_data$species
pair_names <- names(pairwise_alignments(sp))
```

## Step 3. Genes first with `geom_gene()`

The first layer is the gene annotation. There are two kinds of arrows:

- **Context genes** are representative source transcripts outside the
  curated MHC set. They are drawn in neutral grey with `geom_gene()`.
- **Native MHC arrows** are one small arrow per curated MHC gene, using
  the exact curated native span and the source ortholog color. They sit
  in the same lane as the context genes.

Neither layer draws exon glyphs: the transcript structure is used for
model selection, but the figure shows one span per representative gene.
Helper objects such as `strip_labels`, `panel_limits` and `class_spans`
are built a few lines earlier in the renderer and omitted here so that
the layer code stays readable. `models` and `mhc` in the code below are
the transformed tables from Step 2 (the renderer calls the model table
`m`), so their `xmin` and `xmax` columns are already display
coordinates.

``` r

context_arrows <- data.frame(
  track = models$track,
  xmin = models$xmin, xmax = models$xmax, ymin = -0.5,
  transcripts = models$model_id,
  strand = models$strand,
  kind = models$kind
)

native_mhc_arrows <- data.frame(
  track = mhc$track,
  xmin = mhc$xmin, xmax = mhc$xmax, ymin = -0.5,
  transcripts = paste0("curated_native_mhc_", seq_len(nrow(mhc))),
  strand = mhc$display_oriented_strand,
  ortholog_fill = mhc$ortholog_fill
)

p <- ggexon(sp) +
  geom_gene(
    data = context_arrows,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, transcripts = transcripts,
        strand = strand, track = track, fill = kind),
    exon_height = 0.5, colour = NA
  ) +
  geom_gene(
    data = native_mhc_arrows,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, transcripts = transcripts,
        strand = strand, track = track, fill = I(ortholog_fill)),
    exon_height = 0.5, show.legend = FALSE
  )
```

`xmin`/`xmax` are display coordinates, `ymin` is the bottom of the gene
lane, `transcripts` groups the rows, and `strand` controls the arrow
direction. `fill = I(ortholog_fill)` bypasses the ggplot2 color scale,
so each native arrow keeps the exact RGB value from `colors.txt`. The
shared legend is supplied by the heads in the next step, not by this
layer.

At this point `p` contains only the gene arrows: no heads and no
ribbons.

## Step 4. Add the MHC heads with `geom_gene_lollipop()`

Many MHC genes are only a few kilobases long, too small for a readable
label at the native scale. The figure therefore adds one enlarged
directional head per gene, anchored at the midpoint of the same curated
span. The head is a fixed physical size; the small native arrow still
carries the true span.

The curated table is extended with these plotting columns:

| Column | Meaning |
|----|----|
| `true_midpoint` | midpoint of the curated span in display coordinates |
| `display_oriented_strand` | strand flipped for a reverse-oriented track |
| `gene_label` | source gene name, with `*` for a pseudogene |
| `kind` | color/legend key (ortholog class, pseudogene, other gene) |
| `head_row` | one of six shared physical rows for that source gene key |

`head_row` is the key to the layout. Each of the 41 source gene keys is
assigned a fixed row (1-6) from a template, so the same gene appears at
the same height in every haplotype even when its native position
differs. Distinct duplicated copies remain separate heads and are placed
side by side on that row. An empty slot is a layout vacancy, not
evidence that the gene is absent.

``` r

p <- p + geom_gene_lollipop(
  data = mhc,
  aes(x = true_midpoint, y = 0, strand = display_oriented_strand,
      label = gene_label, fill = kind, track = track, head_row = head_row),
  headroom = 8,
  head_layout = "columns",
  head_height_mm = 4,
  padding_mm = 0.6,
  tip_mm = 1.8,
  gap_mm = 0.8,
  row_gap_mm = 0.8,
  stem_height_mm = 1.5,
  family = "Times",
  size = 8 / ggplot2::.pt
)
```

`y = 0` is the gene anchor line and `headroom = 8` is the y-space above
it that the heads may use. The `*_mm` arguments are physical sizes, not
data units, so the heads stay legible when the figure is exported at 180
mm width. The stems rise from the exact midpoint ticks and fan into
their columns; the layout is measured later and converted into panel
heights.

## Step 5. Connect haplotypes with `geom_nuclink()`

The last data layer is the nucleotide links. One
[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
call is added for each adjacent pair of haplotypes, using the stored
`SynPairAlignment`:

``` r

for (nm in pair_names) {
  p <- p + geom_nuclink(
    alignment = nm,
    fill = "grey60",
    colour = NA,
    alpha = 0.3
  )
}
```

Each row of `links.tsv` is one ungapped alignment block: an interval on
the target track (`tspecies`, `tstart`, `tend`) aligned to an interval
on the query track (`qspecies`, `qstart`, `qend`), with `strand` and
`identity`. The layer maps both intervals into the link panel between
the two annotation rows and draws a quadrilateral (four vertices) for
the block. In this figure there are **624 blocks** across the 12
adjacent pairs, all drawn with the same grey fill and transparency.

The blocks were regenerated with minimap2 `asm20` from the exact
extracted native sequences, not taken from the unavailable original Fig.
3c plot data. Blocks are split at indels of at least 1 kb; blocks
shorter than 500 bp or from parent alignments below 70% identity are
omitted for display.

### What the ribbons are - and are not

The ribbons are **pairwise alignments**, not a difference track. They
show where two haplotype sequences align and where the alignment breaks.
They do not measure base-level nucleotide differences, expression, or
repeat copy number. `identity` is stored in the table and could be
mapped to `fill`, but this figure keeps every ribbon grey.

If by “replica sequence” you mean duplicated or repetitive sequence: the
links do not classify repeats. A tandem array or a duplication may
produce repeated or non-colinear blocks, and an indel appears as a break
between blocks, but that is an alignment pattern, not a repeat call.
Repeats and segmental duplications need a separate annotation layer;
they are not inferred from the ribbons.

## Step 6. Tune the link panels with `facet_genomics()`

The script also adds the VR1-VR3 lane and the class I/II spans before
faceting. They are ordinary annotations below the gene anchors:

``` r

p <- p +
  geom_rect(
    data = regions,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey85", colour = NA, inherit.aes = FALSE
  ) +
  geom_segment(
    data = class_spans,
    aes(x = xmin, xend = xmax, y = y, yend = y),
    colour = "grey55", inherit.aes = FALSE
  )
```

The renderer computes the shared VR lane’s `ymin` and `ymax` from
`vr_top` and `vr_height`; `regions` supplies the x intervals.

Faceting now creates one annotation panel per track and one link panel
between each adjacent pair. The ratio between those two panel types is
controlled by `link_panel_height`:

``` r

p <- p +
  facet_genomics(
    vars(track),
    scales = "free",
    ncol = 1,
    strip.position = "left",
    xlim = panel_xlim,
    xlim_chr = panel_chr,
    link_panel_height = 0.35,
    link_axis = "none",
    link_strip = "blank",
    labeller = labeller(track = strip_labels)
  ) +
  scale_panel_annotation("free_y", limits = panel_limits) +
  theme_ggexon_track(base_family = "Times", base_size = 8,
                     show_x_grid = FALSE) +
  theme_ggexon_side_strips("left", base_size = 8)
```

`link_panel_height` is a **relative** height, not a millimetre value. A
positive number is turned into a `null` grid unit relative to an
ordinary panel row, so `0.35` makes each link panel roughly 35% of a
standard annotation panel. Raise it when the ribbons are too flat to
read; lower it when the links dominate the figure. Values around 0.2-0.5
are usually enough. The final export later fixes the link panels at 8 mm
after measuring the heads, because the annotation panels shrink or grow
around their labels.

`scales = "free"` frees the y range for each annotation panel. The x
limits are still the same `[0, max_bp]` range on every track, so the
base-pair scale stays common even though the facet is “free”; the
per-panel y limits are supplied through
[`scale_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_annotation.md).
`link_axis = "none"` removes the axes from the link strips and
`link_strip = "blank"` removes their strip labels. The left-side track
labels come from
[`theme_ggexon_side_strips()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_side_strips.md).

## Step 7. Measure the layout, fix the axes and export

The figure is 180 mm wide, but its height cannot be fixed in advance:
each MHC head contains a label whose rendered width depends on the font
and the number of columns. The renderer therefore draws the gtable once
on a roomy 1000 mm PDF and records every head’s device position through
the `layout_callback` of
[`geom_gene_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_gene_lollipop.md).
It then gives each annotation panel exactly the height its heads need
and gives every link panel a fixed 8 mm.

Native axes are rebuilt in the same pass. The default bottom axis of
each annotation panel is replaced with ticks in that track’s own
one-based assembly coordinates; reverse-oriented tracks count down.
Because the axis grobs are positioned through the shared display
geometry, the tick spacing still proves the common base-pair scale.

``` r

built <- ggexon_build(p)
tab   <- ggexon_gtable(built)

# heads measured, panel heights assigned, native axes installed here

ggsave("mhc_continuous.pdf", tab, width = 180, height = measured_height_mm,
       units = "mm", device = cairo_pdf, bg = "white")
ggsave("mhc_continuous.png", tab, width = 180, height = measured_height_mm,
       units = "mm", dpi = 150, bg = "white")
```

The script fails loudly if the built object does not match the intended
design. Its assertions and exported QC tables check, among other things:

- 13 annotation panels, 12 link panels, no coverage panel;
- one identical bp-per-axis-unit value across all annotation scales;
- 285 heads at the true midpoints, with the recorded strand, label and
  row;
- 624 ribbon blocks, four vertices per block, correct target/query
  ownership;
- the shared VR lane, panel heights, figure dimensions and axis
  round-trips.

The QC outputs are written beside the figure as
`common-bp-scale-qc.tsv`, `native-geometry-qc.tsv`,
`native-axis-qc.tsv`, `annotation-height-qc.tsv`, `link-qc.tsv`,
`layout-qc.tsv` and `figure-dimensions.tsv`, together with the plotted
tables.

### Gorilla class II detail

After composing the genes, heads, links and panel layout, the paired
gorilla class II detail makes the h1/h2 differences easier to inspect:

![Gorilla h2 and h1 class II detail with enlarged heads on shared rows,
true-span gene arrows, grey context and nucleotide
ribbons.](figures/mhc-classii-comparison.png)

## Reading the figure

The small arrows retain each gene’s true curated span and source
ortholog color; the enlarged heads carry the readable labels and
direction on six shared rows. An empty head slot is a layout vacancy,
not evidence that the gene is absent. Occupied slots are read top to
bottom within a column, then left to right.

The grey VR1-VR3 bars are the published CHM13 variable-region intervals
on the human track. On the ape tracks they show only the portions
projected through human-to-ape chain blocks, so they describe shared
comparative regions rather than independent ape-specific discordance.
Asterisks mark pseudogene calls from the source annotation, not
experimental validation.

Context layers labelled “projected” are same-species Liftoff transfers
that passed at least 95% exon coverage and 95% sequence identity. Five
alternative shorter isoforms were recovered at the same threshold; nine
complete models remain unresolved. Recoveries use shorter source
isoforms, and their RNA support is partial and not haplotype-resolved.
Projected context is visualization context, not a complete validated
alternate-haplotype gene catalog, and gene counts have different
ascertainment across released and projected tracks. The region
boundaries and adjacent alignments are documented reconstructions, not
exact reproductions of the source figure.

Both versions are exported at 180 mm width, with height measured from
the head layout. The original full vector PDF remains under
`data-raw/ape_paper/results/mhc_continuous/figures/`; the compact
version is under `data-raw/ape_paper/results/mhc_compact/figures/` and
is shown above.

## Reproducing the compact and full figures

The compact example is reproducible from the whole data object with:

``` sh
Rscript data-raw/ape_paper/render_mhc_compact.R
```

That entry point executes the same read → subset → render sequence shown
above. It reads the checked-in compact direct-alignment table, so
rendering does not require rerunning minimap2 or fetching DNA.
Regenerating those alignments uses
`python3 data-raw/ape_paper/prepare_mhc_compact.py` and requires the
previously acquired regional FASTA caches and minimap2.

The original full view is displayed first above and is also available as
the [13-track
preview](https://dongyaoliu.github.io/ggexon/articles/figures/mhc-continuous.png).
Its bounded inputs and figure are under
`data-raw/ape_paper/results/mhc_continuous/`. From the repository root:

``` sh
Rscript data-raw/ape_paper/render_mhc_continuous.R
```

That command uses the final merged inputs and writes both figure formats
and the QC tables. Rebuilding the inputs from raw sources additionally
requires the bounded fetch, Liftoff projection, minimap2 and recovery
steps documented in
`data-raw/ape_paper/results/mhc_continuous/README.md`. `data-raw/` is
source material and is not installed with the package, so this vignette
embeds the verified previews instead of re-running the source-data
renderer during site builds.
