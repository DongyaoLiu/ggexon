# 1. Projecting amino acid changes onto transcript models

## A dense landscape of HLA-A variation

This example summarizes **5,998 named HLA-A alleles**, an MHC class I
heavy chain, from IPD-IMGT/HLA release **3.66.0**. Comparison with
**A\*02:01:01:01** produces **1,212 amino-acid substitutions at 334
mature-protein residues**. Each substitution has its own head, coloured
by the alternative amino acid’s chemical group. Counts span 1–4,667, so
logarithmic heights keep both rare and common alternatives visible.

The analysis includes every sequence in the pinned release that meets
these explicit criteria:

- A complete 365-residue precursor using only the twenty standard amino
  acids.
- An allele name ending in a numeric field, excluding names with letter
  suffixes.
- The same gap pattern as the reference in the official protein
  alignment.

All 5,998 sequences passing the first two criteria also pass the
alignment check. Thus, equal length is verified against aligned residue
coordinates, rather than assumed to imply positional correspondence.
Partial sequences, other protein lengths, and ambiguous residues are
outside this example’s scope.

**Each named allele counts once**, including named alleles encoding
identical proteins. The panel contains 3,188 distinct mature protein
sequences, but is not deduplicated to those proteins. Counts reflect
database representation, not individuals, independent observations, or
population frequencies.

## Prepare one Syn object

Use one `SynIndividual` for the reference allele throughout this
tutorial. It holds the reference FASTA and exon annotation, the
reference protein, and two attached protein annotations:

- `hla_substitutions`: counts of amino-acid alternatives, keyed to the
  reference transcript.
- `hla_regions`: the six structural regions, using the same transcript
  key.

The example loader reads the bundled offline files, summarizes the
alleles, and attaches these annotations. No separate mutation, domain,
or exon tables need to be prepared before plotting.

``` r

library(ggexon)
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.4.3

demo_dir <- system.file("extdata", "hla_a_lollipop", package = "ggexon")
source(file.path(demo_dir, "load-hla-a-example.R"), local = TRUE)
source(file.path(demo_dir, "plot-hla-a-example.R"), local = TRUE)
source(file.path(demo_dir, "project-hla-a-example.R"), local = TRUE)
hla <- load_hla_a_example(demo_dir)

class(hla)
#> [1] "SynIndividual"
#> attr(,"package")
#> [1] "ggexon"
class(get_annotation(hla, "hla_substitutions"))
#> [1] "SynProteinMutationAnnotation"
#> attr(,"package")
#> [1] "ggexon"
class(get_annotation(hla, "hla_regions"))
#> [1] "SynProteinDomainAnnotation"
#> attr(,"package")
#> [1] "ggexon"
```

`hla` is the object passed to every plot. The allele manifest, original
protein sequences, residue-state counts, and source checksums are
retained in the object’s metadata. `hla_a_evidence(hla)` retrieves them
for inspection. They are supporting evidence, not separate plotting
inputs. The helpers are example functions, rather than exported package
functions.

``` r

data.frame(
  named_alleles = nrow(hla_a_evidence(hla)$alleles),
  distinct_mature_proteins = hla_a_evidence(hla)$distinct_mature_proteins,
  substitutions = nrow(query_protein_mutations(hla, annotation = "hla_substitutions")),
  variable_residues = length(unique(query_protein_mutations(
    hla, annotation = "hla_substitutions"
  )$mature_position))
)
#>   named_alleles distinct_mature_proteins substitutions variable_residues
#> 1          5998                     3188          1212               334
```

The allele manifest retains every exact name and sequence checksum. The
original FASTA records are unchanged and bundled with gzip compression.

### Protein coordinates and structural regions

The original precursor sequences include a 24-residue signal peptide.
The plot uses **mature-protein positions 1–341**, so mature residue 156
is precursor residue 180. The signal peptide is excluded from
comparison, while the original bundled sequences retain it.

The stored mutation `position` and structural `start`/`end` columns use
**precursor coordinates** so they can be projected through the reference
CDS. The same annotations also retain `mature_position`, `mature_start`,
and `mature_end` columns. Selecting these columns gives the
mature-protein view without creating a second set of annotations or
changing the stored coordinates.

Structural boundaries come from the bundled UniProt **P04439** snapshot,
shifted by 24 residues. Its displayed sequence is A\*03:01; only the
shared structural boundaries annotate this equal-length HLA-A track. All
substitutions are computed against the selected A\*02:01 reference.

``` r

knitr::kable(as.data.frame(query_domains(get_annotation(hla, "hla_regions")))[,
  c("domain", "mature_start", "mature_end")],
  col.names = c("Protein region", "Mature start", "Mature end"))
```

| Protein region     | Mature start | Mature end |
|:-------------------|-------------:|-----------:|
| Alpha-1            |            1 |         90 |
| Alpha-2            |           91 |        182 |
| Alpha-3            |          183 |        274 |
| Connecting peptide |          275 |        284 |
| Transmembrane      |          285 |        308 |
| Cytoplasmic        |          309 |        341 |

CP denotes the connecting peptide, TM the transmembrane segment, and
TAIL the cytoplasmic region. These are structural regions rather than
all globular domains. Different DNA variants producing the same
amino-acid change are grouped into one substitution. The protein view
uses amino-acid coordinates; the gene-model section below projects them
to the exact reference allele’s coding structure.

## Dense lollipop summary

``` r

plots <- plot_hla_a_example(hla)
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
plots$overview
```

![A dense HLA-A lollipop landscape containing 1212 substitution heads at
334 residues from 5998 named alleles. Heads are coloured by amino-acid
group, with a logarithmic count axis and labelled structural regions
beneath.](hla-a-lollipop-demo_files/figure-html/hla-a-overview-1.png)

All eligible named HLA-A alleles from the pinned release. Every
substitution remains a separate head; colours describe the alternative
amino acid, not its prevalence.

The plot has three deliberate layout choices:

- **Logarithmic counts.** Head height is `log10(allele_count + 1)`,
  matching ggexon’s `mutation_y_trans = "log10"` convention. Axis ticks
  show the original counts. This prevents thousands of common-allele
  counts from flattening singleton and low-count substitutions into the
  backbone.
- **Bounded local fanning.** Heads at the same residue spread
  symmetrically within at most 1.6 residue units of that position.
  Fanning restarts at each residue, so dense early sites cannot push
  later heads beyond the protein. Every stem remains anchored to the
  actual residue.
- **Chemical-group colour.** The alternative residues are grouped as
  aliphatic (A/V/I/L/M), aromatic (F/W/Y), polar (S/T/N/Q/C), basic
  (K/R/H), acidic (D/E), and glycine/proline (G/P). Exact alternative
  identities remain in the table.

At this density, some nearby heads can visually overlap, especially at
equal counts. No substitutions are filtered or sampled out of the plot.
The close-up and exported table provide exact values when the overview
is too crowded.

The plot starts with `ggexon(hla)`.
[`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md)
resolves the mutation and region annotations from that object during
plot build. The core protein-coordinate recipe is:

``` r

ggexon(hla) +
  geom_protein_lollipop(
    annotation = "hla_substitutions", domain_annotation = "hla_regions",
    position = "mature_position", domain_start = "mature_start", domain_end = "mature_end",
    domain = "domain", protein_length = 341, protein_start = 1,
    mutation_y_by = "allele_count", mutation_y_trans = "log10",
    mutation_y_range = log10(c(1, 4667) + 1), spread_threshold = 0
  )
```

The dense example adds bounded fanning through ggplot2 layer data
functions. Each function receives the plot’s `hla` object, queries its
attached annotation, and computes stems or heads at build time.
`hla_a_lollipop_layout()` applies symmetric offsets and retains every
true residue anchor. This keeps the existing colour and transparency
controls without passing precomputed plotting tables to
[`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md).
Bounded fanning is specific to this example; the package’s default
spreading API is unchanged.

``` r

head(query_protein_mutations(hla, annotation = "hla_substitutions")[,
  c("mutation", "allele_count", "n_alternatives", "amino_acid_group")])
#>   mutation allele_count n_alternatives amino_acid_group
#> 1      G1A            1              4        Aliphatic
#> 2      G1D            3              4           Acidic
#> 3      G1S            1              4            Polar
#> 4      G1V            4              4        Aliphatic
#> 5      S2C            2              4            Polar
#> 6      S2F            3              4         Aromatic
```

## Multiple alternatives at residue 156

The reference has **L** at mature residue 156. In this expanded panel
there are **five** alternative amino acids, each counted separately:

``` r

focus_states <- subset(hla_a_evidence(hla)$states, position == 156)
knitr::kable(focus_states[, c("ref", "alt", "is_reference", "allele_count")],
             col.names = c("Reference", "Observed", "Reference match", "Named alleles"))
```

|     | Reference | Observed | Reference match | Named alleles |
|:----|:----------|:---------|:----------------|--------------:|
| 856 | L         | F        | FALSE           |             2 |
| 857 | L         | L        | TRUE            |          3172 |
| 858 | L         | M        | FALSE           |             1 |
| 859 | L         | Q        | FALSE           |          1184 |
| 860 | L         | R        | FALSE           |           596 |
| 861 | L         | W        | FALSE           |          1043 |

``` r

stopifnot(sum(focus_states$allele_count) == nrow(hla_a_evidence(hla)$alleles))
```

There are **3,172 reference matches**, plus **L156F (2)**, **L156M
(1)**, **L156Q (1,184)**, **L156R (596)**, and **L156W (1,043)**. These
sum to 5,998. Reference matches remain in the state table and are not
substitution heads.

``` r

plots$focus
```

![Five separate heads for L156F, L156M, L156Q, L156R and L156W share the
true residue 156 anchor. Their respective labelled counts are 2, 1,
1184, 596 and
1043.](hla-a-lollipop-demo_files/figure-html/hla-a-multiallelic-focus-1.png)

Five amino-acid alternatives sharing one residue anchor. Heights use the
same log10(count + 1) convention as the overview; labels show exact
original counts.

Each summary row includes up to three example allele names. The complete
manifest and source sequences retain all eligible alleles without
producing unwieldy lists of thousands of names in each table cell.

## Project onto the reference exon–intron model

The same 1,212 substitutions can be placed above the **eight coding
exons and seven introns** of A\*02:01:01:01. The bundled IPD HLA00005
reference snapshot comes from the same database release. Its spliced CDS
translates exactly to our 365-residue reference precursor followed by a
stop codon.

The genomic view uses **allele-local HLA00005 coordinates**, not GRCh38
chromosome coordinates. It shows the coding region from ATG to stop;
untranslated flanks are outside the displayed model. Other alleles’
substitutions are projected onto this one reference structure, not onto
separate allele-specific intron models.

``` r

knitr::kable(hla_a_gene_structure(hla),
  col.names = c("Feature", "Number", "Start", "End", "Length (bp)"))
```

|     | Feature | Number | Start |  End | Length (bp) |
|:----|:--------|-------:|------:|-----:|------------:|
| 1   | exon    |      1 |   301 |  373 |          73 |
| 9   | intron  |      1 |   374 |  503 |         130 |
| 2   | exon    |      2 |   504 |  773 |         270 |
| 10  | intron  |      2 |   774 | 1014 |         241 |
| 3   | exon    |      3 |  1015 | 1290 |         276 |
| 11  | intron  |      3 |  1291 | 1890 |         600 |
| 4   | exon    |      4 |  1891 | 2166 |         276 |
| 12  | intron  |      4 |  2167 | 2265 |          99 |
| 5   | exon    |      5 |  2266 | 2382 |         117 |
| 13  | intron  |      5 |  2383 | 2820 |         438 |
| 6   | exon    |      6 |  2821 | 2853 |          33 |
| 14  | intron  |      6 |  2854 | 2995 |         142 |
| 7   | exon    |      7 |  2996 | 3043 |          48 |
| 15  | intron  |      7 |  3044 | 3212 |         169 |
| 8   | exon    |      8 |  3213 | 3217 |           5 |

### Store the genomic projection

The exon model and `hla_substitutions` are already attached to `hla`.
Run the projection verb before building the plot. With `store = TRUE`,
it returns the updated Syn object and attaches a named genomic
annotation:

``` r

hla <- project_mutations_to_genome(
  hla, annotation = "hla_substitutions", transcripts = "HLA_A0201_tx",
  store = TRUE, name = "hla_genomic"
)
class(get_annotation(hla, "hla_genomic"))
#> [1] "SynGenomicProjectionAnnotation"
#> attr(,"package")
#> [1] "ggexon"
```

**This verb converts amino-acid positions to genomic base pairs.** It
reads precursor positions and follows the selected transcript’s CDS
blocks. The original protein annotation, including its mature-coordinate
columns, remains available for the preceding protein view.

For a codon crossing a splice junction, the stored projection retains
each exonic fragment and one marker anchor on the longest fragment. For
example:

**Mature residue 1 → precursor residue 25 → genomic fragments 373 and
504–505.**

Inspect the stored result directly, without projecting again:

``` r

knitr::kable(unique(subset(
  query_genomic_projection(hla, "hla_genomic"), mature_position == 1,
  select = c("mature_position", "position", "seqnames", "xmin", "xmax")
)), col.names = c("Mature residue", "Precursor residue", "Reference sequence",
                  "Genomic start (bp)", "Genomic end (bp)"), row.names = FALSE)
```

| Mature residue | Precursor residue | Reference sequence | Genomic start (bp) | Genomic end (bp) |
|---:|---:|:---|---:|---:|
| 1 | 25 | HLA00005 | 373 | 373 |
| 1 | 25 | HLA00005 | 504 | 505 |

The marker anchor for this split codon is **504.5**, the centre of bases
504–505. It lies on coding sequence even though the codon crosses an
intron.

### Prepare one layout for every layer

Prepare intron spacing and head positions on the same object.
`intron_factor` sets the shared exon-union map; `head_spread` bounds the
sideways fanning of alternatives sharing an anchor. The verb computes
the stems from those same anchors and head positions. Heights retain the
existing `log10(count + 1)` convention; the exon model sits below count
1.

``` r

hla <- prepare_genomic_layout(
  hla, annotation = "hla_genomic", name = "hla_lollipops",
  intron_factor = 3, head_spread = 4.8,
  mutation_y_by = "allele_count", mutation_y_trans = "log10",
  mutation_y_range = log10(c(1, 4667) + 1),
  track_ymin = -0.34, track_ymax = -0.08, stem_points = 24
)
```

The stored layout keeps original genomic coordinates plus one shared
display map. Its head, stem, and exon tables can be inspected with
`query_genomic_layout(hla, "hla_lollipops", component = "mutations")`
(or `"stems"` and `"exons"`). These are views of prepared state, rather
than separate inputs that the reader must construct.

### Draw the prepared result

All spatial layers and the x scale select
`genomic_layout = "hla_lollipops"`. They read the prepared geometry
without projecting or spreading positions again. The shared map is
applied once across the plot.

First define the amino-acid colours and the counts to label on the y
axis:

``` r

aa_colours <- c(
  Aliphatic = "#E69F00", Aromatic = "#56B4E9", Polar = "#009E73",
  Basic = "#CC79A7", Acidic = "#D55E00", `Gly / Pro` = "#0072B2"
)
count_breaks <- c(1, 10, 100, 1000, 5000)
```

Build the plot directly from `hla`.
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
draws the prepared exon model;
[`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md)
draws its anchored stems and heads. The exon-number labels read the
stored exon coordinates from the same object. The code below includes
all colours, axis labels, and theme settings used in the rendered
figure:

``` r

genomic_plot <- ggexon(hla) +
  geom_exon2(
    genomic_layout = "hla_lollipops",
    intron_shape = "chevron", intron_peak = 0.12,
    fill = "#263D52", colour = "#263D52", linewidth = 0.35,
    transcript_backbone_colour = "#647E8A", show.legend = FALSE
  ) +
  geom_protein_lollipop(
    genomic_layout = "hla_lollipops", score = "amino_acid_group",
    show_backbone = FALSE, show_domains = FALSE,
    stem_colour = NULL, stem_linewidth = 0.14, stem_alpha = 0.27,
    point_size = 0.65, point_alpha = 0.9
  ) +
  geom_text(
    data = function(model) query_genomic_layout(model, "hla_lollipops", "exons"),
    aes(x = (xmin + xmax) / 2, label = exon_number),
    y = -0.53, size = 8 / ggplot2::.pt, colour = "#344854",
    inherit.aes = FALSE
  ) +
  scale_x_ggexon_genomic(
    genomic_layout = "hla_lollipops",
    guide = guide_x_ggexon_piecewise(by = "panel")
  ) +
  scale_colour_manual(
    "Alternative amino acid", values = aa_colours,
    breaks = names(aa_colours), drop = FALSE
  ) +
  scale_y_continuous(
    breaks = log10(count_breaks + 1),
    labels = scales::label_comma()(count_breaks),
    limits = c(-0.8, log10(5001) + 0.18),
    expand = expansion(mult = c(0.035, 0.08))
  ) +
  labs(
    title = "HLA-A  /  variants on the gene model",
    subtitle = "5,998 named alleles    /    1,212 substitutions    /    8 coding exons + 7 introns    /    introns ÷ 3",
    x = "Reference allele A*02:01:01:01  /  HLA00005 allele-local genomic coordinates",
    y = "Allele count (log scale)",
    caption = paste0(
      "Stem anchors follow the reference CDS, including splice-split codons. ",
      "The numbered coding-exon model spans ATG to stop.\n",
      "Exons at native scale; intron gaps ÷ 3. Scale bars show actual base-pair lengths. ",
      "Database counts, not population frequencies."
    )
  ) +
  guides(colour = guide_legend(
    nrow = 1, override.aes = list(size = 2.4, alpha = 1)
  )) +
  theme_minimal(base_size = 10, base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5E9EC", linewidth = 0.3),
    axis.text = element_text(colour = "#5E6C78"),
    axis.title = element_text(colour = "#344854"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.title = element_text(
      size = 23, face = "bold", colour = "#203747", margin = margin(b = 7)
    ),
    plot.subtitle = element_text(size = 11, colour = "#5E6C78", margin = margin(b = 18)),
    plot.caption = element_text(
      size = 9, hjust = 0, colour = "#697884", margin = margin(t = 14)
    ),
    plot.title.position = "plot", plot.caption.position = "plot",
    legend.position = "bottom", legend.title = element_text(face = "bold"),
    legend.key.width = grid::unit(9, "mm"),
    plot.margin = margin(16, 18, 12, 14)
  )
genomic_plot
```

![The 1212 HLA-A substitution lollipops projected above eight numbered
coding exons and seven visible introns. Introns are compressed
three-fold while exon widths retain their native scale. Separate exon
and intron scale bars show their different display
scales.](hla-a-lollipop-demo_files/figure-html/hla-a-gene-plot-1.png)

Amino-acid positions are first projected through the A\*02:01:01:01
transcript’s CDS to genomic codon positions. The genomic x scale then
displays each intron at one-third its original length while preserving
exon lengths.

Here
[`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md)
uses the stored **genomic** layout. Its ordinary mode still draws
protein coordinates; the selected layout makes this choice explicit.
Optional `geom_mutation_label(genomic_layout = "hla_lollipops")` reads
the same head positions, so labels cannot acquire a different spread.

Change intron compression or head spacing by running
[`prepare_genomic_layout()`](https://dongyaoliu.github.io/ggexon/reference/prepare_genomic_layout.md)
again. The scale reads the prepared map, and exon layers disable their
own schematic compression. An intron of 600 bp occupies 200 exon-scale
units; a 276-bp exon still occupies 276. Use `intron_factor = 1` during
preparation for an uncompressed view. Changing source mutations or CDS
structure requires projection and layout preparation again; stale
prepared state raises an error.

Rectangles run from `start - 0.5` to `end + 0.5`, the edges of the first
and last base. This makes a 73-base exon exactly 73 coordinate units
wide and the first intron exactly 130, avoiding an off-by-one error from
inclusive endpoints. Codon anchors retain their original genomic
base-centre coordinates. The guide reports **73 bp** for the
representative exon and **130 bp** for the intron.

## Reproduce and customize

From the repository root:

``` sh
python3 data-raw/hla_a_lollipop/build-hla-a-demo.py
Rscript data-raw/hla_a_lollipop/verify-hla-a-demo.R
Rscript data-raw/hla_a_lollipop/verify-hla-a-gene-model.R
Rscript data-raw/hla_a_lollipop/render-hla-a-figure.R
```

The builder pins the source commit and checks all selected sequences
against the official gapped alignment. The renderer exports overview and
close-up and gene-model **vector PDF and PNG** files, substitution
counts, projected codon segments, and all residue-state counts to
`data-raw/hla_a_lollipop/preview`.

Edit `hla_a_plot_parameters` in `plot-hla-a-example.R` to adjust canvas
dimensions, font size, head size, stem opacity, or the maximum local fan
width. The default 240 × 135 mm canvas provides space for the dense
overview. Every substitution is drawn in both the vector and raster
exports.

### Sources and attribution

- [IPD-IMGT/HLA](https://www.ebi.ac.uk/ipd/imgt/hla/) release 3.66.0,
  [pinned source
  repository](https://github.com/ANHIG/IMGTHLA/tree/d972ae287948dc2e5468a40ff3b3d27227541189).
  Original FASTA records retain their headers, sequences, and line
  wrapping. `IPD-LICENCE.md` retains the data terms; the package MIT
  license does not replace these terms. Counts are computed analyses of
  original records.
- Barker DJ et al. *The IPD-IMGT/HLA Database: recent developments in
  sequence submission*. Nucleic Acids Research (2026), 54(D1):
  D1152–D1158.
- Robinson J, Barker D, Marsh SGE. *25 years of the IPD-IMGT/HLA
  Database*. HLA (2024), 103(6): e15549.
- Robinson J et al. *IMGT/HLA — a sequence database for the human major
  histocompatibility complex*. Tissue Antigens (2000), 55:280–287.
- [UniProt P04439](https://www.uniprot.org/uniprotkb/P04439/entry):
  shared structural regions, under [CC BY
  4.0](https://www.uniprot.org/help/license). The original snapshot and
  source SHA-256 values are bundled.
- [IPD alignment
  conventions](https://www.ebi.ac.uk/ipd/imgt/hla/alignment/help/):
  mature-protein numbering and alignment symbols.
