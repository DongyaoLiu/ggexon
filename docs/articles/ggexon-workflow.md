# 0. Basic usage

## From data to a genomic figure

Genomic figures often combine exon positions on a chromosome, links
between genomes, signal depth, and positions along a protein. ggexon
connects these views through stored annotations and a ggplot2 plotting
grammar.

Each example below presents the input, constructs or prepares the ggexon
object, and chooses geoms, scales, and a theme. The small gene models,
gene-order matrix, protein counts, and DNA strings are synthetic
teaching data. The pairwise, synteny, and coverage examples use bundled
files. Run the chunks in order in an R session with the current version
of ggexon installed.

``` r

library(ggexon)
library(ggplot2)
```

### Major data objects

| Object | What it stores | Typical input |
|----|----|----|
| `SynFeatureAnnotation` | Genes, transcripts, exons, and CDS | GFF3 or GTF |
| `SynIndividual` | One genome or sample and its annotation layers | Annotation, optional FASTA |
| `SynSpecies` | Individuals and comparative relationships | Individuals, alignments, homology, trees |
| `SynPairAlignment`, `SynMultiAlignment` | Relationships between genomes | PAF/PSL, or supported multiple-alignment data |
| `SynBigWigAnnotation`, `SynVCFAnnotation` | Region-queryable signal or variants | BigWig or VCF |
| `SynProteinDomainAnnotation`, `SynProteinMutationAnnotation` | Domains or mutation summaries | Domain and mutation tables |
| `SynAnnotationPatch` | An explicit gene-model correction | Corrected feature ranges |
| `HomologyAnnotation` | Reference/query gene mappings | Curated table or imported BLAST results |
| `SynLayout`, `SynLocusSet` | Reusable panel windows or comparable loci | Named coordinate windows |

### Plotting functions supplied by ggexon

[`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md)
starts the figure. Syn-aware layers resolve stored annotations when the
plot is built; table-based layers use prepared data. Ordinary ggplot2
scales and themes remain available. A theme changes appearance; it does
not select a genomic window or transform coordinates.

#### Geoms

| Function | What it draws |
|----|----|
| [`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md) | Exon rectangles, transcript backbones, and strand direction |
| [`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md) | Exon/CDS/UTR models with explicit introns and optional schematic compression |
| [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md) | One directional gene span, with optional labels and overlap lanes |
| [`geom_genebox()`](https://dongyaoliu.github.io/ggexon/reference/geom_genebox.md) | Equal-size gene boxes with internal strand arrows |
| [`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md) | Gene labels and their leader lines, including homology-based names |
| [`geom_genelink()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelink.md) | Straight, elbow, or spline lines between supplied coordinate pairs |
| [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md) | Nucleotide-alignment ribbons between genomic panels |
| [`geom_synteny_link()`](https://dongyaoliu.github.io/ggexon/reference/geom_synteny_link.md) | Interval ribbons for supplied gene, exon, or block relationships |
| [`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md) | Continuous coverage or signal tracks |
| [`geom_motif()`](https://dongyaoliu.github.io/ggexon/reference/geom_motif.md) | Motif/domain blocks; attached protein domains can be projected onto genomic CDS intervals |
| [`geom_aa_variant()`](https://dongyaoliu.github.io/ggexon/reference/geom_aa_variant.md) | Amino-acid variant markers projected onto transcript models |
| [`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md) | A protein backbone, domain blocks, and mutation stems and heads |
| [`geom_mutation_label()`](https://dongyaoliu.github.io/ggexon/reference/geom_mutation_label.md) | Mutation labels positioned to match mutation markers |
| [`geom_alignseq()`](https://dongyaoliu.github.io/ggexon/reference/geom_alignseq.md) | Anchored DNA-difference markers or aligned sequence rows |
| [`geom_genomic_tree()`](https://dongyaoliu.github.io/ggexon/reference/geom_genomic_tree.md) | Attaches a rectangular tree for alignment with genomic panels |

#### Scales and guides

| Function | What it controls |
|----|----|
| [`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md) | Shared exon-aware intron compression with original genomic coordinate labels |
| [`guide_x_ggexon_piecewise()`](https://dongyaoliu.github.io/ggexon/reference/guide_x_ggexon_piecewise.md) | Representative exon and intron scale bars for the compressed genomic x scale |
| [`strip_scale_x()`](https://dongyaoliu.github.io/ggexon/reference/strip_scale_x.md) | Gene-order display widths or explicit shared comparison slots |
| [`strip_scale()`](https://dongyaoliu.github.io/ggexon/reference/strip_scale_x.md) | Compatibility alias for [`strip_scale_x()`](https://dongyaoliu.github.io/ggexon/reference/strip_scale_x.md) |
| [`scale_x_facet_labels()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_facet_labels.md) | Different x-axis labels for facets sharing a continuous x scale |
| [`scale_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_annotation.md) | Shared or independent annotation-panel y scales, with optional visible limits |
| [`scale_panel_coverage()`](https://dongyaoliu.github.io/ggexon/reference/scale_panel_coverage.md) | Shared or independent coverage-panel y scales, with optional manual or automatic visible limits |

#### Facets and panel layout

| Function | What it arranges |
|----|----|
| [`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md) | Annotation, coverage, and intervening link panels |
| [`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md) | Genomic panels ordered by tree tips, beside one spanning tree |
| [`center_panel_annotation()`](https://dongyaoliu.github.io/ggexon/reference/center_panel_annotation.md) | Centers annotation bodies in their panel views |
| [`set_panel_xlim()`](https://dongyaoliu.github.io/ggexon/reference/set_panel_xlim.md), [`clear_panel_xlim()`](https://dongyaoliu.github.io/ggexon/reference/clear_panel_xlim.md) | Store or clear per-individual genomic windows |
| [`effective_panel_windows()`](https://dongyaoliu.github.io/ggexon/reference/effective_panel_windows.md) | Inspect the annotation windows resolved for a plot or stored layout |
| [`locus_grid_layout()`](https://dongyaoliu.github.io/ggexon/reference/locus_grid_layout.md), [`use_locus_grid()`](https://dongyaoliu.github.io/ggexon/reference/use_locus_grid.md) | Prepare and apply layouts for comparable locus windows |
| [`annotate_cross_panel()`](https://dongyaoliu.github.io/ggexon/reference/annotate_cross_panel.md) | Connect explicitly supplied anchors across panels |
| [`link_panels()`](https://dongyaoliu.github.io/ggexon/reference/link_panels.md) | Connect matching IDs represented by anchors in different panels |

#### Themes

| Function | When to use it |
|----|----|
| [`theme_ggexon_base()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_base.md) | A background-free foundation retaining ordinary axes and grids |
| [`theme_ggexon_clean()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_clean.md) | The named shortcut for that clean background-free styling |
| [`theme_ggexon_track()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_track.md) | Compact genomic tracks, with control over axes, grids, and legends |
| [`theme_ggexon_pairwise()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_pairwise.md) | Stacked annotation/link/annotation comparisons |
| [`theme_ggexon_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_track.md) | Genomic tracks aligned beside a tree |
| [`theme_ggexon_side_strips()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_side_strips.md) | Horizontal facet labels on the left or right, added after a complete theme |

#### Plot preparation and output

| Function | What it prepares |
|----|----|
| [`alignseq_data()`](https://dongyaoliu.github.io/ggexon/reference/alignseq_data.md) | Anchored columns and difference events from two already aligned DNA strings |
| [`protein_lollipop_data()`](https://dongyaoliu.github.io/ggexon/reference/protein_lollipop_data.md) | Protein backbone, domain, and mutation layout tables |
| [`protein_mutation_data()`](https://dongyaoliu.github.io/ggexon/reference/protein_mutation_data.md) | Access the normalized table in a protein-mutation annotation |
| [`syn_gr_to_exon_df()`](https://dongyaoliu.github.io/ggexon/reference/syn_gr_to_exon_df.md) | Convert genomic feature ranges to an exon plotting table |
| [`project_domains_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_domains_to_genome.md), [`project_mutations_to_genome()`](https://dongyaoliu.github.io/ggexon/reference/project_mutations_to_genome.md) | Map protein positions through transcript CDS blocks to genomic intervals |
| [`ggexon_output_size()`](https://dongyaoliu.github.io/ggexon/reference/ggexon_output_size.md) | Record the intended figure dimensions for downstream layout |

Functions such as
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html),
[`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html),
[`scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html),
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
[`coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html),
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html), and
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) come
from ggplot2. The examples combine these with the ggexon functions
above.

### Curating elements within data objects

Curation verbs describe the change made to stored biological
information. Updating selected fields differs from replacing a complete
gene model; resetting patches restores the base annotation.

| Operation | Function | Effect |
|----|----|----|
| Add relationships | [`add_homology()`](https://dongyaoliu.github.io/ggexon/reference/homology-crud.md) | Insert query-to-reference gene relationships |
| Update fields | [`update_homology()`](https://dongyaoliu.github.io/ggexon/reference/homology-crud.md) | Change supplied columns for matched query genes while preserving other fields |
| Remove relationships | [`remove_homology()`](https://dongyaoliu.github.io/ggexon/reference/homology-crud.md) | Remove selected homology rows, with an optional reference-gene guard |
| Set display labels | [`set_gene_labels()`](https://dongyaoliu.github.io/ggexon/reference/set_gene_labels.md) | Assign labels while retaining stable biological IDs |
| Map identifiers | [`map_domain_ids()`](https://dongyaoliu.github.io/ggexon/reference/map_domain_ids.md) | Map domain annotation identifier values to the key used by transcript models |
| Patch gene models | [`patch_annotation()`](https://dongyaoliu.github.io/ggexon/reference/patch_annotation.md) | Record an addition, replacement, or removal using `mode = "add"`, `"replace"`, or `"remove"` |
| Reset patches | [`reset_patches()`](https://dongyaoliu.github.io/ggexon/reference/reset_patches.md) | Restore the base feature annotation and retain assigned display labels |

[`replace_homology()`](https://dongyaoliu.github.io/ggexon/reference/homology-crud.md),
[`delete_homology()`](https://dongyaoliu.github.io/ggexon/reference/homology-crud.md),
[`rename_domain_annotation_ids()`](https://dongyaoliu.github.io/ggexon/reference/map_domain_ids.md),
and
[`clear_patches()`](https://dongyaoliu.github.io/ggexon/reference/reset_patches.md)
remain supported aliases for the corresponding operations above. Patch
mode `"drop"` remains an alias for `"remove"`. Use `query_*()` to
inspect data and `subset_*()` or `filter_*()` to select the data to
keep.

## 1. Exons and alternative transcripts

An exon table contains intervals and transcript membership. Drawing each
row as an unrelated rectangle loses introns and strand direction.
[`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md)
uses the feature hierarchy to draw transcript models.

### Present the input

Create two synthetic genes on `chr1`. Gene A has a full transcript and
an exon-skipping transcript; gene B is on the opposite strand.
Coordinates are one-based and inclusive, as in GTF. These are teaching
models, not observed gene annotations.

``` r

features <- data.frame(
  type = c("gene", "transcript", rep("exon", 3), rep("CDS", 3),
           "transcript", rep("exon", 2), rep("CDS", 2),
           "gene", "transcript", "exon", "exon"),
  start = c(101, 101, 101, 501, 901, 101, 501, 901,
            101, 101, 901, 101, 901, 1201, 1201, 1201, 1501),
  end = c(960, 960, 160, 590, 960, 160, 590, 960,
          960, 160, 960, 160, 960, 1560, 1560, 1260, 1560),
  strand = c(rep("+", 13), rep("-", 4)),
  gene_id = c(rep("geneA", 13), rep("geneB", 4)),
  transcript_id = c(NA, rep("A_full", 7), rep("A_skip", 5),
                    NA, rep("B_only", 3))
)
knitr::kable(features)
```

| type       | start |  end | strand | gene_id | transcript_id |
|:-----------|------:|-----:|:-------|:--------|:--------------|
| gene       |   101 |  960 | \+     | geneA   | NA            |
| transcript |   101 |  960 | \+     | geneA   | A_full        |
| exon       |   101 |  160 | \+     | geneA   | A_full        |
| exon       |   501 |  590 | \+     | geneA   | A_full        |
| exon       |   901 |  960 | \+     | geneA   | A_full        |
| CDS        |   101 |  160 | \+     | geneA   | A_full        |
| CDS        |   501 |  590 | \+     | geneA   | A_full        |
| CDS        |   901 |  960 | \+     | geneA   | A_full        |
| transcript |   101 |  960 | \+     | geneA   | A_skip        |
| exon       |   101 |  160 | \+     | geneA   | A_skip        |
| exon       |   901 |  960 | \+     | geneA   | A_skip        |
| CDS        |   101 |  160 | \+     | geneA   | A_skip        |
| CDS        |   901 |  960 | \+     | geneA   | A_skip        |
| gene       |  1201 | 1560 | \-     | geneB   | NA            |
| transcript |  1201 | 1560 | \-     | geneB   | B_only        |
| exon       |  1201 | 1260 | \-     | geneB   | B_only        |
| exon       |  1501 | 1560 | \-     | geneB   | B_only        |

Write the records as a standard nine-column GTF. Substitute your own GTF
or GFF3 path when working with real annotations.

``` r

attributes <- paste0('gene_id "', features$gene_id, '";')
has_transcript <- !is.na(features$transcript_id)
attributes[has_transcript] <- paste0(
  attributes[has_transcript], ' transcript_id "',
  features$transcript_id[has_transcript], '";'
)
gtf <- data.frame(
  seqname = "chr1", source = "synthetic", type = features$type,
  start = features$start, end = features$end, score = ".",
  strand = features$strand,
  phase = ifelse(features$type == "CDS", "0", "."), attributes = attributes
)
gtf_file <- tempfile(fileext = ".gtf")
write.table(gtf, gtf_file, sep = "\t", quote = FALSE,
            row.names = FALSE, col.names = FALSE)
```

### Construct and inspect the object

`SynIndividual(annotation_file = ...)` attaches a default
`SynFeatureAnnotation`. A FASTA is optional for annotation plotting;
sequence extraction and translation require one.
[`load_annotation()`](https://dongyaoliu.github.io/ggexon/reference/load_annotation.md)
materializes the features so that we can inspect them before drawing.

``` r

model <- SynIndividual(
  annotation_file = gtf_file, annotation_format = "gtf", id = "teaching"
)
model <- load_annotation(model)
class(model)
#> [1] "SynIndividual"
#> attr(,"package")
#> [1] "ggexon"
class(get_annotation(model))
#> [1] "SynFeatureAnnotation"
#> attr(,"package")
#> [1] "ggexon"
exons <- query_features(model, feature_type = "exon", all = TRUE)
knitr::kable(as.data.frame(exons)[,
  c("seqnames", "start", "end", "strand", "gene_id", "transcript_id")])
```

| seqnames | start |  end | strand | gene_id | transcript_id |
|:---------|------:|-----:|:-------|:--------|:--------------|
| chr1     |   101 |  160 | \+     | geneA   | A_full        |
| chr1     |   501 |  590 | \+     | geneA   | A_full        |
| chr1     |   901 |  960 | \+     | geneA   | A_full        |
| chr1     |   101 |  160 | \+     | geneA   | A_skip        |
| chr1     |   901 |  960 | \+     | geneA   | A_skip        |
| chr1     |  1201 | 1260 | \-     | geneB   | B_only        |
| chr1     |  1501 | 1560 | \-     | geneB   | B_only        |

### Choose the geom, scales, and theme

Map transcript IDs to fill to distinguish isoforms. The ordinary
continuous x scale preserves genomic spacing. The track theme hides the
y axis used to position models; `show_legend = TRUE` retains the
transcript legend. Use `transcript_arrow_length = 30` for a fixed 30-bp
horizontal triangle width on every transcript. `transcript_arrow_ratio`
controls the triangle’s height relative to exon height, rather than its
length along the x axis.

``` r

transcript_colours <- c(A_full = "#0072B2", A_skip = "#D55E00", B_only = "#009E73")
p_exons <- ggexon(model) +
  geom_exon(
    aes(fill = transcript_id), chr = "chr1", subset = c(80, 1600),
    transcript_arrow_length = 30
  ) +
  scale_fill_manual(values = transcript_colours, name = "Transcript") +
  scale_x_continuous(breaks = c(100, 500, 900, 1300, 1600),
                     labels = scales::label_number(big.mark = ",")) +
  theme_ggexon_track(base_size = 10, show_legend = TRUE)
p_exons
```

![Synthetic exon models: A full has three exons, A skip omits the middle
exon, and B only is on the minus
strand.](ggexon-workflow_files/figure-html/workflow-exons-1.png)

The x distances are base pairs. The missing middle exon in `A_skip`
comes from the input annotation; ggexon does not infer alternative
splicing.

### Compare the same annotation with other geoms

The same `model` can support several views. Keep chromosome 1 and the
same genomic window, then choose a geom according to the detail you want
to show.

#### `geom_exon2()`: exon and intron structure

[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
keeps the three transcript rows and draws introns as chevrons between
exon blocks. Reuse the transcript colours from `p_exons`.
`compress_introns = FALSE` preserves genomic distances for this
comparison; the next example shows its schematic compression option.

``` r

p_exon2 <- ggexon(model) +
  geom_exon2(
    aes(fill = transcript_id), chr = "chr1", subset = c(80, 1600),
    compress_introns = FALSE, arrow_width = 12
  ) +
  scale_fill_manual(values = transcript_colours, name = "Transcript") +
  scale_x_continuous(limits = c(80, 1600),
                     breaks = c(100, 500, 900, 1300, 1600),
                     labels = scales::label_number(big.mark = ",")) +
  theme_ggexon_track(base_size = 10, show_legend = TRUE)
p_exon2
```

![The same three synthetic transcripts drawn with geom_exon2: gene A has
full and exon-skipping isoforms, gene B is on the minus strand, and
chevron introns retain genomic
spacing.](ggexon-workflow_files/figure-html/workflow-geom-exon2-1.png)

##### A compact transcript schematic

Long introns can dominate a transcript diagram and make exon structure
hard to see. Use `compress_introns = TRUE` when you want a compact
cartoon of exon order and exon skipping. Exon widths are retained, while
`intron_width = 50` replaces each intron with a 50-unit schematic gap.

``` r

p_exon2_schematic <- ggexon(model) +
  geom_exon2(
    aes(fill = transcript_id), chr = "chr1", subset = c(80, 1600),
    compress_introns = TRUE, intron_width = 50, arrow_width = 12
  ) +
  scale_fill_manual(values = transcript_colours, name = "Transcript") +
  theme_ggexon_track(
    base_size = 10, show_legend = TRUE,
    show_x_axis = FALSE, show_x_grid = FALSE
  )
p_exon2_schematic
```

![The same three transcripts drawn as compact schematics with
fixed-width intron gaps. The x axis and grid are hidden because the
transcripts no longer share genomic
spacing.](ggexon-workflow_files/figure-html/workflow-geom-exon2-schematic-1.png)

Each transcript is rearranged independently, so shared exons may no
longer line up across isoforms. Hide the x axis and grid because these
schematic positions do not represent shared genomic coordinates. The
next section uses
[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md)
to compress introns while keeping transcripts and other genomic layers
aligned.

#### `geom_genetag()`: whole-gene spans

[`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md)
draws one directional span per gene. Gene A’s two isoforms therefore
become one gene-level shape, and individual exons are no longer shown.
Map fill to `gene_id` and use a gene palette for the two genes. Set
`arrow_width = 50` to give both strand-direction triangles the same
50-bp width, independent of gene length. Set `ylim` in
[`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)
to leave vertical space around the tags.

``` r

gene_colours <- c(geneA = "#0072B2", geneB = "#009E73")
p_gene_spans <- ggexon(model) +
  geom_genetag(
    aes(fill = gene_id), chr = "chr1", subset = c(80, 1600),
    show_label = FALSE, arrow_width = 50
  ) +
  scale_fill_manual(values = gene_colours, name = "Gene") +
  scale_x_continuous(limits = c(80, 1600),
                     breaks = c(100, 500, 900, 1300, 1600),
                     labels = scales::label_number(big.mark = ",")) +
  coord_cartesian(ylim = c(-1, 2)) +
  theme_ggexon_track(base_size = 10, show_legend = TRUE)
p_gene_spans
```

![Gene A and gene B drawn as whole-gene tags with equal 50-bp
strand-direction triangles and vertical space around them. Gene A points
right and gene B points left; their total widths retain the annotated
gene
spans.](ggexon-workflow_files/figure-html/workflow-geom-genetag-fixed-arrow-1.png)

#### `geom_genebox()`: gene positions and strand

[`geom_genebox()`](https://dongyaoliu.github.io/ggexon/reference/geom_genebox.md)
draws equal-size squares with internal strand arrows. Query the same
model’s gene rows and place each square at the midpoint of its annotated
span. Using this table includes both genes, including gene B, which has
no CDS records in the teaching annotation.

Here `box_size = 120` sets a fixed display size, and
`coord_fixed(ratio = 1)` keeps the boxes square. Their positions remain
genomic coordinates; their widths do not represent gene lengths. The
hidden y axis only positions the boxes.

``` r

gene_rows <- as.data.frame(query_features(
  model, chr = "chr1", start = 80, end = 1600, feature_type = "gene"
))
gene_rows$x <- (gene_rows$start + gene_rows$end) / 2
gene_rows$y <- 0

p_gene_boxes <- ggexon(gene_rows) +
  geom_genebox(aes(x = x, y = y, strand = strand, fill = gene_id),
               box_size = 120) +
  scale_fill_manual(values = gene_colours, name = "Gene") +
  scale_x_continuous(limits = c(80, 1600),
                     breaks = c(100, 500, 900, 1300, 1600),
                     labels = scales::label_number(big.mark = ",")) +
  coord_fixed(ratio = 1, ylim = c(-120, 120)) +
  theme_ggexon_track(base_size = 10, show_legend = TRUE)
p_gene_boxes
```

![Equal-size blue and green squares at the annotated midpoints of gene A
and gene B. Internal arrows indicate opposite strands, while the x axis
retains genomic
coordinates.](ggexon-workflow_files/figure-html/workflow-geom-genebox-1.png)

## 2. Compress introns while retaining genomic labels

Long introns can make exons too small to compare. A shared exon-aware
scale compresses gaps while keeping transcripts aligned, helping you
compare exon structure across isoforms.

### Present the input

Reuse the synthetic GTF and inspect gene A’s exon rows.

``` r

knitr::kable(features[features$gene_id == "geneA" & features$type == "exon", ])
```

|     | type | start | end | strand | gene_id | transcript_id |
|:----|:-----|------:|----:|:-------|:--------|:--------------|
| 3   | exon |   101 | 160 | \+     | geneA   | A_full        |
| 4   | exon |   501 | 590 | \+     | geneA   | A_full        |
| 5   | exon |   901 | 960 | \+     | geneA   | A_full        |
| 10  | exon |   101 | 160 | \+     | geneA   | A_skip        |
| 11  | exon |   901 | 960 | \+     | geneA   | A_skip        |

### Prepare the object

Create a copy restricted to the first locus, leaving `model` reusable.

``` r

gene_a <- subset_individual(model, chr = "chr1", start = 80, end = 980)
query_features(gene_a, feature_type = "exon", all = TRUE)
#> GRanges object with 5 ranges and 7 metadata columns:
#>       seqnames    ranges strand |    source        type     score       phase
#>          <Rle> <IRanges>  <Rle> |  <factor> <character> <numeric> <character>
#>   [1]     chr1   101-160      + | synthetic        exon        NA        <NA>
#>   [2]     chr1   501-590      + | synthetic        exon        NA        <NA>
#>   [3]     chr1   901-960      + | synthetic        exon        NA        <NA>
#>   [4]     chr1   101-160      + | synthetic        exon        NA        <NA>
#>   [5]     chr1   901-960      + | synthetic        exon        NA        <NA>
#>           gene_id transcript_id   gene_name
#>       <character>   <character> <character>
#>   [1]       geneA        A_full       geneA
#>   [2]       geneA        A_full       geneA
#>   [3]       geneA        A_full       geneA
#>   [4]       geneA        A_skip       geneA
#>   [5]       geneA        A_skip       geneA
#>   -------
#>   seqinfo: 1 sequence from an unspecified genome; no seqlengths
```

### Choose the geom, scales, and theme

[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
draws explicit chevron introns.
[`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md)
applies a shared transformation to the original genomic coordinates,
taking precedence over the geom’s schematic compression. A factor of 8
displays eligible gaps at one eighth of their genomic length while
preserving exon widths.

``` r

p_compressed <- ggexon(gene_a) +
  geom_exon2(
    aes(fill = transcript_id), chr = "chr1", compress_introns = FALSE,
    arrow_width = 12
  ) +
  scale_x_ggexon_genomic(intron_factor = 8, n.breaks = 4) +
  scale_fill_manual(values = transcript_colours, name = "Transcript") +
  theme_ggexon_track(base_size = 10, show_legend = TRUE)
p_compressed
```

![Two gene A transcripts with chevron introns compressed eightfold and
original genomic tick
labels.](ggexon-workflow_files/figure-html/workflow-compressed-exons-1.png)

Ticks report original genomic positions, but equal display distances
across exons and introns no longer imply equal base-pair distances. A
piecewise guide can show separate representative exon and intron scale
bars. The solid exon bar and dashed intron bar share one horizontal
baseline; labels show their original lengths in bp. Use `linewidth` to
set the thickness of both bars and their end ticks in millimetres.

``` r

ggexon(gene_a) +
  geom_exon2(
    aes(fill = transcript_id), chr = "chr1", compress_introns = FALSE,
    arrow_width = 12
  ) +
  scale_x_ggexon_genomic(
    intron_factor = 8,
    guide = guide_x_ggexon_piecewise(by = "panel", linewidth = 0.6)
  ) +
  scale_fill_manual(values = transcript_colours) +
  theme_ggexon_track(base_size = 10)
```

![The compressed gene A models with solid exon and dashed intron scale
bars on one horizontal baseline, labeled only with their original
lengths in
bp.](ggexon-workflow_files/figure-html/workflow-piecewise-guide-aligned-1.png)

With explicit CDS and UTR records, `geom_exon2(annotation_type = "all")`
can distinguish their box heights.

## 3. Gene spans and readable labels

For a locus overview, transcript detail can hide gene order. A
directional tag gives one span per gene. Separate labels keep names
readable without replacing the stable gene IDs used in computation.

### Present the input

``` r

knitr::kable(features[features$type == "gene",
                      c("gene_id", "start", "end", "strand")])
```

|     | gene_id | start |  end | strand |
|:----|:--------|------:|-----:|:-------|
| 1   | geneA   |   101 |  960 | \+     |
| 14  | geneB   |  1201 | 1560 | \-     |

``` r

labels <- c(geneA = "Gene A (two isoforms)", geneB = "Gene B")
```

### Prepare the object

[`set_gene_labels()`](https://dongyaoliu.github.io/ggexon/reference/set_gene_labels.md)
returns an updated object carrying plot labels alongside the original
IDs.

``` r

labelled_model <- set_gene_labels(model, labels)
```

### Choose the geom, scales, and theme

Suppress built-in tag labels and add
[`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md)
for explicit placement. `collapse_tandem = TRUE` also combines adjacent
genes sharing a label. Use the same x limits, `c(80, 1600)`, and y
limits, `c(-1, 2)`, as in `p_gene_spans` to keep the gene bodies near
the middle of the plot. Set `fontface = "italic"` in
[`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md)
to italicize the displayed gene names.

``` r

p_gene_labels <- ggexon(labelled_model) +
  geom_genetag(
    aes(fill = gene_id), chr = "chr1", subset = c(80, 1600),
    show_label = FALSE, arrow_width = 50
  ) +
  geom_genelabel(
    chr = "chr1", subset = c(80, 1600), label_direction = "top",
    size = 3, fontface = "italic", collapse_tandem = TRUE,
    panel_width_inch = 7
  ) +
  scale_fill_manual(values = gene_colours) +
  scale_x_continuous(limits = c(80, 1600),
                     breaks = c(100, 500, 900, 1300, 1600),
                     labels = scales::label_number(big.mark = ",")) +
  coord_cartesian(ylim = c(-1, 2)) +
  theme_ggexon_track(base_size = 10)
p_gene_labels
```

![Gene A and gene B shown near the middle of the plot with italic gene
labels above their directional spans. The x limits and vertical padding
match the preceding gene-tag example, and both triangles have a fixed
50-bp
width.](ggexon-workflow_files/figure-html/workflow-genetags-italic-centred-1.png)

For crowded loci, `gene_layout = "stack"` or `"nested"` separates
overlapping bodies. For cross-species names, attach a
`HomologyAnnotation` to a `SynSpecies` and use it through
[`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md).
Labels alone do not establish homology.

## 4. Pairwise nucleotide alignments

Two genomes can contain the same locus at different chromosome
positions. `SynSpecies` stores their annotations and alignment together;
[`facet_genomics()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomics.md)
places links between their source tracks.

### Present the input

Use the bundled XZ1516 and N2 annotations and a small PAF alignment.
This example needs neither a download nor user-supplied files.

``` r

compact_dir <- system.file("extdata", "compact_synspecies", package = "ggexon")
xz_gff <- file.path(compact_dir, "caenorhabditis_XZ1516.gff3")
n2_gtf <- file.path(compact_dir, "c_elegans.PRJNA13758.WS285.canonical_geneset.gtf")
paf_file <- file.path(compact_dir, "V_alginment.paf")
stopifnot(file.exists(xz_gff), file.exists(n2_gtf), file.exists(paf_file))
cat(readLines(xz_gff, n = 5), sep = "\n")
#> ##gff-version 3
#> III_RagTag   funannotate gene    448 2094    .   -   .   ID=FUN_000001;
#> III_RagTag   funannotate mRNA    448 2094    .   -   .   ID=FUN_000001-T1;Parent=FUN_000001;product=hypothetical protein;
#> III_RagTag   funannotate exon    1994    2094    .   -   .   ID=FUN_000001-T1.exon1;Parent=FUN_000001-T1;
#> III_RagTag   funannotate exon    735 1344    .   -   .   ID=FUN_000001-T1.exon2;Parent=FUN_000001-T1;
cat(readLines(n2_gtf, n = 3), sep = "\n")
#> V    WormBase    gene    20448270    20453184    .   -   .   gene_id "WBGene00009848"; gene_version "1"; gene_source "WormBase"; gene_biotype "protein_coding"; gene_name "F48F5.6";
#> V    WormBase    transcript  20448275    20449959    .   -   .   gene_id "WBGene00009848"; gene_version "1"; transcript_id "F48F5.6a.1"; gene_source "WormBase"; gene_biotype "protein_coding"; transcript_source "WormBase"; transcript_biotype "protein_coding";
#> V    WormBase    exon    20449750    20449959    .   -   .   gene_id "WBGene00009848"; gene_version "1"; transcript_id "F48F5.6a.1"; exon_number "1"; gene_source "WormBase"; gene_biotype "protein_coding"; transcript_source "WormBase"; transcript_biotype "protein_coding"; exon_id "F48F5.6a.1.e1";
raw_paf <- read.delim(paf_file, header = FALSE, comment.char = "")
names(raw_paf)[1:12] <- c(
  "query", "qlen", "qstart", "qend", "strand", "target", "tlen",
  "tstart", "tend", "matches", "alen", "mapq"
)
knitr::kable(head(raw_paf[, c("query", "qstart", "qend", "strand", "alen")], 4))
```

| query    |   qstart |     qend | strand | alen |
|:---------|---------:|---------:|:-------|-----:|
| V_RagTag | 21580812 | 21581913 | \+     | 1104 |
| V_RagTag | 21581910 | 21584356 | \-     | 2552 |
| V_RagTag | 21579732 | 21580830 | \-     | 1131 |
| V_RagTag | 21574445 | 21579192 | \-     | 5159 |

``` r

knitr::kable(head(raw_paf[, c("target", "tstart", "tend", "matches", "mapq")], 4))
```

| target |   tstart |     tend | matches | mapq |
|:-------|---------:|---------:|--------:|-----:|
| V      | 18218473 | 18219520 |     793 |  255 |
| V      | 20456948 | 20459445 |    2225 |  255 |
| V      | 20459441 | 20460565 |    1025 |  255 |
| V      | 20460561 | 20465040 |    3806 |  255 |

Raw PAF uses zero-based starts and half-open intervals. The ggexon
importer normalizes returned start/end columns to one-based closed
intervals. Let it perform this conversion once.

### Construct and inspect the objects

``` r

xz <- SynIndividual(annotation_file = xz_gff, id = "XZ1516")
n2 <- SynIndividual(annotation_file = n2_gtf, annotation_format = "gtf", id = "N2")
comparison <- SynSpecies(name = "Caenorhabditis pair")
comparison <- add_individual(comparison, xz)
comparison <- add_individual(comparison, n2)
comparison <- add_pairwise_alignment(
  comparison,
  SynPairAlignment(
    name = "XZ1516_vs_N2", query_individual = "XZ1516",
    target_individual = "N2", file = paf_file, format = "paf"
  )
)
individual_names(comparison)
#> [1] "XZ1516" "N2"
aligned_blocks <- pairwise_alignment_data(
  comparison, alignment = "XZ1516_vs_N2",
  subset = c(XZ1516 = "RagTag_V:21574445-21584356"), filter = 200
)
knitr::kable(head(aligned_blocks[, intersect(
  c("qname", "qstart", "qend", "tname", "tstart", "tend", "strand", "alen"),
  names(aligned_blocks)
)], 6))
```

|   qstart |     qend |   tstart |     tend | strand | alen |
|---------:|---------:|---------:|---------:|:-------|-----:|
| 21580813 | 21581913 | 18218474 | 18219520 | \+     | 1104 |
| 21581911 | 21584356 | 20456949 | 20459445 | \-     | 2552 |
| 21579733 | 21580830 | 20459442 | 20460565 | \-     | 1131 |
| 21574446 | 21579192 | 20460562 | 20465040 | \-     | 5159 |

`filter = 200` removes short alignment fragments from this query result,
leaving the stored alignment unchanged.
[`subset_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/subset_pairwise_alignment.md)
and
[`filter_pairwise_alignment()`](https://dongyaoliu.github.io/ggexon/reference/filter_pairwise_alignment.md)
expose these operations separately.

### Choose the geom, scales, facet, and theme

[`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md)
derives a comparison window from XZ1516 coordinates and resolves the
corresponding N2 region. Independent x ranges show each genome’s own
positions. The annotation y policy controls model layout. Hide the link
panel’s axes because the ribbons connect the two annotation rulers
rather than defining a third genomic coordinate system.

``` r

p_pairwise <- ggexon(comparison) +
  geom_exon(species = c("N2", "XZ1516"), fill = "#0072B2") +
  geom_nuclink(
    reference = "XZ1516", chr = "RagTag_V",
    subset = c(21574445, 21584356), alignment = "XZ1516_vs_N2",
    fill = "#56B4E9", alpha = 0.55
  ) +
  facet_genomics(vars(track), scales = "free_x", ncol = 1,
                 strip.position = "left", vertical = "center",
                 link_axis = "none", link_strip = "blank", link_panel_height = 0.6) +
  scale_panel_annotation("fixed_y") +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  theme_ggexon_pairwise(base_size = 10)
p_pairwise
```

![N2 and XZ1516 exon models connected by nucleotide alignment
ribbons.](ggexon-workflow_files/figure-html/workflow-pairwise-plot-1.png)

The GFF uses `V_RagTag`; the resolver also accepts the `RagTag_V` alias
used in the plotting call. Inspect seqnames when preparing your own
files.

Omitting `species` on a `SynSpecies` exon layer selects all stored
individuals. Select it explicitly for a subset. For repeated figures,
[`set_panel_xlim()`](https://dongyaoliu.github.io/ggexon/reference/set_panel_xlim.md)
stores named per-individual windows on the comparison’s `SynLayout`;
[`clear_panel_xlim()`](https://dongyaoliu.github.io/ggexon/reference/clear_panel_xlim.md)
removes the overrides.

`SynMultiAlignment` supports ODGI-backed comparisons. It can store
already parsed MAF data through `data`, but
[`load_alignment()`](https://dongyaoliu.github.io/ggexon/reference/load_alignment.md)
does not parse MAF files. See the [alignment class
reference](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md)
for supported inputs.

## 5. Local synteny from real alignment blocks

Reuse the `comparison` object from the previous example. Gene tags show
the annotated gene spans, and
[`geom_synteny_link()`](https://dongyaoliu.github.io/ggexon/reference/geom_synteny_link.md)
connects the stored PAF alignment intervals. Both layers read from the
same `SynSpecies` object.

### Present the input

The `comparison` object already contains the XZ1516 GFF3, N2 WS285 GTF,
and `XZ1516_vs_N2` alignment. Select a local window in each assembly
using its chromosome name: `V_RagTag` for XZ1516 and `V` for N2.

### Set the windows on the object

[`store_chain_layout()`](https://dongyaoliu.github.io/ggexon/reference/store_chain_layout.md)
creates the annotation-link-annotation panel layout.
[`set_panel_xlim()`](https://dongyaoliu.github.io/ggexon/reference/set_panel_xlim.md)
then stores each assembly’s window on that layout. The geoms use these
windows to select gene annotations and alignment blocks when the plot is
built.

``` r

local_synteny <- store_chain_layout(
  comparison, free = list(x = TRUE, y = FALSE)
)
local_synteny <- set_panel_xlim(
  local_synteny, individual = c("XZ1516", "N2"),
  xlim = list(XZ1516 = c(21574000, 21585000), N2 = c(20456500, 20465500)),
  xlim_chr = c(XZ1516 = "V_RagTag", N2 = "V")
)
effective_panel_windows(local_synteny)
#>   PANEL  track panel_type individual      chr    start      end
#> 1     1 XZ1516 annotation     XZ1516 V_RagTag 21574000 21585000
#> 3     3     N2 annotation         N2        V 20456500 20465500
```

Two N2 genes and four XZ1516 genes lie in these windows. Three alignment
blocks remain, all in reverse orientation. The fourth PAF hit maps
outside the selected N2 locus and is excluded by the panel windows. This
creates a new object carrying the local layout; `comparison` remains
available for other views.

### Choose the geom, scales, facet, and theme

Pass `local_synteny` directly to
[`ggexon()`](https://dongyaoliu.github.io/ggexon/reference/ggexon.md).
Use grey tags for the genes and one blue fill for all alignment ribbons.
`gene_layout = "stack"` separates the overlapping N2 gene spans.
Built-in tag labels use the annotation names, with
`label_fontface = "italic"` and `label_position = "outside"` to place
them above the gene bodies. Independent base-pair axes retain each
assembly’s coordinates; the middle panel has no genomic axis of its own.

``` r

p_synteny <- ggexon(local_synteny) +
  geom_genetag(
    fill = "grey75", colour = "grey35", arrow_width = 100,
    gene_layout = "stack", label_size = 2.5,
    label_fontface = "italic", label_position = "outside"
  ) +
  geom_synteny_link(
    alignment = "XZ1516_vs_N2", fill = "#56B4E9", alpha = 0.5
  ) +
  facet_genomics(vars(track), scales = "free_x", ncol = 1,
                 strip.position = "left", vertical = "center",
                 link_axis = "none", link_strip = "blank", link_panel_height = 0.6) +
  scale_panel_annotation("fixed_y") +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  theme_ggexon_pairwise(base_size = 10)
p_synteny
```

![Real XZ1516 and N2 gene annotations resolved directly from a
SynSpecies object and joined by three blue reverse-oriented PAF
alignment ribbons. Four XZ1516 genes appear above two overlapping N2
gene spans. Grey tags show gene spans and italic labels give annotation
names.](ggexon-workflow_files/figure-html/workflow-synteny-synspecies-1.png)

The ribbons show sequence-alignment spans, not independently curated
gene-to-gene orthology assignments. A ribbon may cross several gene
tags.
[`geom_synteny_link()`](https://dongyaoliu.github.io/ggexon/reference/geom_synteny_link.md)
resolves the alignment stored in `local_synteny` without inferring gene
relationships. Neither layer needs a separately prepared plotting table.

## 6. RNA-seq coverage beside gene annotation

RNA-seq coverage and gene models share genomic coordinates but use
different y scales. Here forward- and reverse-strand testis RNA-seq from
*D. sechellia* occupy separate panels above the two gene models,
*CG42797* and *Ptpmeg2*. Both signal annotations and the gene models
belong to one individual in a `SynSpecies` object.

### Present the input

Reuse the bundled GTF and forward- and reverse-strand BigWigs from the
[local synteny
tutorial](https://dongyaoliu.github.io/ggexon/articles/dox-synteny-demo.md).
These files use chromosome-X coordinates on the *D. sechellia* assembly
GCF_004382195.1. Keep the full tutorial window, with a one-base display
margin, to include both genes. The GTF contains CDS and UTR pieces for
the two mapped transcript models. The BigWig backend runs on macOS and
Linux, and is unsupported on Windows.

``` r

rna_dir <- system.file("extdata", "dox_synteny", "objects", package = "ggexon")
rna_gtf <- file.path(rna_dir, "Dsech.gtf")
rna_plus_file <- file.path(rna_dir, "Dsech.RNAplus.bw")
rna_minus_file <- file.path(rna_dir, "Dsech.RNAminus.bw")
rna_window <- c(9396968L, 9444822L)
cat(readLines(rna_gtf, n = 8), sep = "\n")
#> ##gff-version 2
#> ##source-version rtracklayer 1.66.0
#> ##date 2026-10-27
#> X    ggexon_curated_Dox_demo UTR 9396969 9397323 .   +   .   gene_id "Dsech_CG42797"; gene_name "CG42797"; transcript_id "Dsech_NM_132346.3"
#> X    ggexon_curated_Dox_demo gene    9396969 9410256 .   +   .   gene_id "Dsech_CG42797"; gene_name "CG42797";
#> X    ggexon_curated_Dox_demo transcript  9396969 9410256 .   +   .   gene_id "Dsech_CG42797"; gene_name "CG42797"; transcript_id "Dsech_NM_132346.3"
#> X    ggexon_curated_Dox_demo CDS 9397324 9397422 .   +   .   gene_id "Dsech_CG42797"; gene_name "CG42797"; transcript_id "Dsech_NM_132346.3"
#> X    ggexon_curated_Dox_demo CDS 9397483 9397637 .   +   .   gene_id "Dsech_CG42797"; gene_name "CG42797"; transcript_id "Dsech_NM_132346.3"
```

The two BigWigs retain the deposited RNA-seq signal scores from testis
biological replicate 1. They are not raw read counts. `RNA_plus`
represents the forward transcript strand; `RNA_minus` stores the
reverse-strand scores as positive values. *CG42797* is on the plus
strand and *Ptpmeg2* is on the minus strand.

### Construct and inspect the objects

``` r

rna_individual <- SynIndividual(
  id = "Dsech", annotation_file = rna_gtf, annotation_format = "gtf",
  metadata = list(assembly = "GCF_004382195.1")
)
rna_individual <- add_annotation(rna_individual, SynBigWigAnnotation(
  name = "RNA_plus", bigwig_file = rna_plus_file,
  metadata = list(transcript_strand = "+", signal_unit = "deposited signal")
))
rna_individual <- add_annotation(rna_individual, SynBigWigAnnotation(
  name = "RNA_minus", bigwig_file = rna_minus_file,
  metadata = list(transcript_strand = "-", signal_unit = "deposited signal",
                  score_handling = "positive reverse-strand scores")
))
rna_comparison <- add_individual(
  SynSpecies(name = "D. sechellia testis RNASeq"), rna_individual
)
individual_names(rna_comparison)
#> [1] "Dsech"

head(as.data.frame(query_signal(
  get_annotation(rna_individual, "RNA_minus"), chr = "X",
  start = rna_window[1], end = rna_window[2]
)))
#>   seqnames   start     end width strand score
#> 1        X 9398376 9398376     1      *  0.99
#> 2        X 9398377 9398377     1      *  0.99
#> 3        X 9398378 9398378     1      *  0.99
#> 4        X 9398379 9398379     1      *  0.99
#> 5        X 9398380 9398380     1      *  0.99
#> 6        X 9398381 9398381     1      *  0.99
```

[`query_signal()`](https://dongyaoliu.github.io/ggexon/reference/query_signal.md)
inspects the stored signal as intervals with a `score`. The returned
coordinates are 1-based inclusive. This inspection is separate from
plotting: each geom below reads directly from `rna_comparison`.

### Give each strand its own panel

Store a three-row layout on the object. `track` names the panels; the
`individual` and `species` columns connect each row to the stored *D.
sechellia* individual. `RNASeq +` and `RNASeq -` are two coverage-panel
aliases for that same individual. The gene annotation occupies the third
panel, with the `Dsech` ID. All three rows share the same genomic
window.

``` r

species_layout(rna_comparison) <- SynLayout(
  panels = data.frame(
    PANEL = 1:3, ROW = 1:3, COL = 1L,
    track = c("RNASeq +", "RNASeq -", "Dsech"),
    panel_type = c("coverage", "coverage", "annotation"),
    individual = c("Dsech", "Dsech", NA_character_),
    species = c(NA_character_, NA_character_, "Dsech"),
    xlim_chr = "X", xlim_min = rna_window[1], xlim_max = rna_window[2]
  ),
  free = list(x = FALSE, y = TRUE)
)
```

### Choose the geom, scales, facet, and theme

Select a panel alias with `species` in each
[`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md)
layer and choose its stored BigWig with `annotation`. This routes the
forward signal to `RNASeq +` and the reverse signal to `RNASeq -`, while
both layers read from the same individual.
`scale_panel_coverage("free_y")` gives each panel its own y-range and
axis. Both signals retain their original scores.

``` r

rna_gene_colours <- c(CG42797 = "#0072B2", Ptpmeg2 = "#009E73")
```

`geom_exon2(annotation_type = "all")` draws thick CDS and thinner UTR
pieces, with genomic intron lengths retained. Both annotation layers
explicitly select `Dsech`. Gene models and coverage share an
uncompressed genomic x-axis.

``` r

coverage_plot <- ggexon(rna_comparison) +
  geom_coverage(
    species = "RNASeq +", annotation = "RNA_plus", fill = "#0072B2", alpha = 0.8
  ) +
  geom_coverage(
    species = "RNASeq -", annotation = "RNA_minus", fill = "#009E73", alpha = 0.8
  ) +
  geom_exon2(
    aes(ymin = 1, fill = gene_name, colour = gene_name),
    species = "Dsech", chr = "X", subset = rna_window, annotation_type = "all",
    exon_height = 0.45, cds_height = 1, utr_height = 0.45,
    compress_introns = FALSE, arrow_width = 450, linewidth = 0.2,
    show.legend = FALSE
  ) +
  geom_genelabel(
    aes(ymin = 1, colour = gene_name), species = "Dsech", chr = "X",
    subset = rna_window, exon_height = 0.45, label_direction = "bottom",
    label_offset_fraction = 0.12,
    fontface = "italic", size = 3, show_link = FALSE, show.legend = FALSE
  ) +
  facet_genomics(vars(track), ncol = 1, strip.position = "left",
                 coverage_panel_height = 1 / 3,
                 labeller = as_labeller(c("RNASeq +" = "RNASeq +",
                                          "RNASeq -" = "RNASeq -",
                                          Dsech = "D. sechellia"))) +
  scale_panel_annotation("free_y", limits = list(Dsech = c(0.84, 1.48))) +
  scale_panel_coverage("free_y") +
  scale_fill_manual(values = rna_gene_colours, guide = "none") +
  scale_colour_manual(values = rna_gene_colours, guide = "none") +
  scale_x_continuous(limits = rna_window,
                     breaks = seq(9400000, 9440000, by = 10000),
                     labels = scales::label_number(scale = 1e-6, suffix = " Mb",
                                                    accuracy = 0.01),
                     oob = scales::oob_keep) +
  scale_y_continuous(
    name = "RNASeq signal", breaks = seq(0, 150, by = 25),
    guide = guide_axis(check.overlap = TRUE),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(x = "Chromosome X position") +
  theme_ggexon_track(base_size = 10, show_y_axis = TRUE,
                     show_x_grid = FALSE) +
  theme_ggexon_side_strips("left", base_size = 10) +
  theme(axis.title.x = element_text(), axis.title.y = element_text(),
        panel.spacing.y = grid::unit(6, "pt"))
coverage_plot
```

![A compact stack of three aligned D. sechellia panels. RNASeq + is blue
and RNASeq - is green, each with its own y-axis and unmodified scores.
The CG42797 and Ptpmeg2 models sit directly beneath the RNASeq tracks,
with little vertical padding and italic names below their gene bodies.
Each RNASeq panel is one-third the annotation panel's
height.](ggexon-workflow_files/figure-html/workflow-coverage-dsech-tight-annotation-1.png)

The panel labels identify the strands, so a separate legend is
unnecessary. There is no magnification or secondary-axis transformation.
Because the coverage panels have independent y-ranges, equal heights in
the two panels need not represent equal signal values; read each panel’s
own tick labels.

`show_y_axis = TRUE` keeps the signal ticks. The annotation y-range
closely brackets the gene bodies and their labels below, bringing the
models close to the RNASeq tracks. A small panel gap keeps adjacent axis
labels readable. `coverage_panel_height = 1 / 3` makes each RNASeq panel
one-third as tall as the annotation panel, giving the three panels a
height ratio of **1:1:3**. `oob_keep` retains boundary intervals for
clipping at the genomic window’s edge.

Use `scale_panel_coverage("fixed_y")` instead if you want all coverage
panels to share one signal range, so equal heights represent equal
values. This example retains a linear genomic x-axis to align RNASeq and
annotation. The [Dox
tutorial](https://dongyaoliu.github.io/ggexon/articles/dox-synteny-demo.html#data-provenance-and-interpretation)
describes the source library, deposited signals, and coverage checks.

## 7. Place protein mutation labels on genomic gene models

Plot amino-acid substitutions along a protein to see where they fall
within protein domains. Lollipop heights show the number of samples
carrying each substitution. We first summarize substitutions in protein
coordinates, then place their labels on genomic gene models.

### Present the input

Protein positions and genomic positions describe different coordinate
systems. For this example, create a synthetic mutation summary for a
160-residue protein, plus two synthetic domain intervals. These are
teaching data, not biological claims. Counts describe samples carrying
each mutation.

``` r

raw_mutations <- data.frame(
  gene = "demo-protein",
  event_type = "substitution",
  mutation_hash_or_notation = c("A#18#V", "K#42#R", "G#88#D", "S#140#F"),
  sample_count = c(2L, 5L, 12L, 20L)
)
protein_domains <- data.frame(
  gene_id = "demo-protein", protein_length = 160L,
  start = c(10L, 85L), end = c(60L, 145L),
  Domain = c("Binding", "Catalytic")
)
raw_mutations
#>           gene   event_type mutation_hash_or_notation sample_count
#> 1 demo-protein substitution                    A#18#V            2
#> 2 demo-protein substitution                    K#42#R            5
#> 3 demo-protein substitution                    G#88#D           12
#> 4 demo-protein substitution                   S#140#F           20
protein_domains
#>        gene_id protein_length start end    Domain
#> 1 demo-protein            160    10  60   Binding
#> 2 demo-protein            160    85 145 Catalytic
```

The mutation reader normalizes `gene` to `gene_id`, matching the domain
table’s key. `protein_length` records the full 160-residue protein; the
last mutation at residue 140 does not define its end. Protein
coordinates here are one-based amino-acid positions.

### Construct and inspect the object

``` r

mutation_file <- tempfile(fileext = ".tsv")
domain_file <- tempfile(fileext = ".tsv")
write.table(raw_mutations, mutation_file, sep = "\t", quote = FALSE, row.names = FALSE)
write.table(protein_domains, domain_file, sep = "\t", quote = FALSE, row.names = FALSE)
protein_individual <- add_annotation(
  SynIndividual(id = "synthetic-cohort"),
  SynProteinMutationAnnotation(
    name = "mutation_counts", mutation_file = mutation_file, keytype = "gene_id"
  )
)
protein_individual <- add_annotation(protein_individual, SynProteinDomainAnnotation(
  name = "protein_domains", domain_file = domain_file, keytype = "gene_id"
))
class(protein_individual)
#> [1] "SynIndividual"
#> attr(,"package")
#> [1] "ggexon"
query_protein_mutations(
  protein_individual, annotation = "mutation_counts", genes = "demo-protein"
)[, c("gene_id", "position", "ref", "alt", "mutation", "sample_count")]
#>        gene_id position ref alt mutation sample_count
#> 1 demo-protein       18   A   V     A18V            2
#> 2 demo-protein       42   K   R     K42R            5
#> 3 demo-protein       88   G   D     G88D           12
#> 4 demo-protein      140   S   F    S140F           20
```

The object owns both annotations. Loading is deferred until inspection
or plotting; mutation queries parse hash notation such as `A#18#V` into
a position, reference residue, alternate residue, and label. The query
above inspects the input. Neither plotting layer below needs its
returned table.

### Choose the geom, scales, and theme

At plot build,
[`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md)
reads `mutation_counts`, selects `demo-protein`, matches its domains in
`protein_domains`, and prepares the backbone, stems, and heads. The
stored protein length sets the backbone end. Here the target height
range equals the input count range, so the identity transform preserves
the observed sample counts as head heights.

``` r

count_range <- range(raw_mutations$sample_count)
```

``` r

protein_plot <- ggexon(protein_individual) +
  geom_protein_lollipop(
    annotation = "mutation_counts", domain_annotation = "protein_domains",
    genes = "demo-protein",
    mutation_y_by = "sample_count", mutation_y_range = count_range,
    track_ymin = 0.60, track_ymax = 0.68,
    domain_default_ymin = 0.55, domain_default_ymax = 0.75,
    stem_linewidth = 0.9,
    point_colour = "#222222", point_size = 2.5
  ) +
  geom_mutation_label(
    annotation = "mutation_counts", genes = "demo-protein",
    mutation_y_by = "sample_count",
    mutation_y_range = count_range, label_nudge_y = 0,
    angle = 0, vjust = -0.6, size = 3
  ) +
  scale_fill_manual(values = c(Binding = "#0072B2", Catalytic = "#E69F00")) +
  scale_x_continuous(breaks = seq(0, 160, 40), limits = c(0, 160)) +
  scale_y_log10(breaks = c(1, 2, 5, 10, 20), limits = c(0.4, 28)) +
  labs(x = "Amino-acid position", y = "Samples with mutation (log10 scale)",
       fill = "Domain") +
  theme_ggexon_track(base_size = 10, show_y_axis = TRUE, show_legend = TRUE) +
  theme(axis.title = element_text())
protein_plot
```

![Synthetic protein domains with thicker grey stems and four mutation
lollipops showing sample counts of two, five, twelve, and twenty on a
logarithmic
y-axis.](ggexon-workflow_files/figure-html/workflow-protein-plot-1.png)

The label layer selects the same annotation and gene, and repeats the
head-height settings so each label follows its mutation.
[`scale_y_log10()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
changes the axis spacing while keeping tick labels in sample counts. The
backbone and domains sit below count 1 at positive layout coordinates;
their vertical positions do not encode counts. A log scale requires
positive values. `label_nudge_y = 0` anchors labels at their heads, and
`vjust = -0.6` keeps the text above them. `stem_linewidth = 0.9`
thickens the grey stems. Keeping `show_y_axis = TRUE` matters here
because y carries a measured quantity. With the default lollipop height
range, heights would be rescaled layout positions and should not be
labeled as raw counts. No genomic x scale or facet is needed for this
single protein-coordinate track.

For transcript- or protein-specific records, set the annotations’
`keytype` to `"transcript_id"` or `"protein_id"` and give both geoms the
same `ids` selector. Select one protein per individual; multiple
proteins have separate coordinate systems. A `SynSpecies` can hold
several individuals, selected with `species` and displayed with
`facet_genomics(vars(track))`.

When a domain table does not contain `protein_length`, the geom can use
one matching sequence stored in
[`protein_seq()`](https://dongyaoliu.github.io/ggexon/reference/protein_seq.md),
excluding a terminal stop character. Otherwise supply `protein_length`
explicitly. It does not estimate the full length from the last observed
mutation.

### Project protein features onto the genomic model

A protein residue number is not a chromosome coordinate. Projection
follows the transcript’s CDS blocks, so one protein interval can occupy
several exons. This example reconnects gene A’s isoforms with a
synthetic genome, then places a domain and a mutation on the exon models
in genomic base pairs.

#### Present the raw data

Reuse `gtf_file` from the first example. Its full transcript contains
three CDS blocks; `A_skip` uses the first and last. Fill a 1,700-base
`chr1` with unknown bases, then insert complete coding blocks. The first
block starts with `ATG`, the last ends with `TAA`, and all CDS phases
are zero.

``` r

coding_blocks <- data.frame(
  start = c(101L, 501L, 901L), end = c(160L, 590L, 960L),
  dna = c(paste0("ATG", strrep("GCC", 19)), strrep("GCC", 30),
          paste0(strrep("GCC", 19), "TAA"))
)
coding_blocks$width <- nchar(coding_blocks$dna)
knitr::kable(coding_blocks[, c("start", "end", "width")])
```

| start | end | width |
|------:|----:|------:|
|   101 | 160 |    60 |
|   501 | 590 |    90 |
|   901 | 960 |    60 |

``` r

domain_input <- data.frame(
  transcript_id = "A_full", domain = "Domain",
  start = 15L, end = 30L, analysis = "Pfam"
)
mutation_input <- data.frame(
  gene_id = "geneA", transcript_id = "A_full",
  position = 25L, ref = "A", alt = "V",
  sample_count = 4L, mutation = "A25V", source_row_id = 1L
)
knitr::kable(domain_input)
```

| transcript_id | domain | start | end | analysis |
|:--------------|:-------|------:|----:|:---------|
| A_full        | Domain |    15 |  30 | Pfam     |

``` r

knitr::kable(mutation_input)
```

| gene_id | transcript_id | position | ref | alt | sample_count | mutation | source_row_id |
|:--------|:--------------|---------:|:----|:----|-------------:|:---------|--------------:|
| geneA   | A_full        |       25 | A   | V   |            4 | A25V     |             1 |

Domain bounds and mutation positions above are one-based **amino-acid**
coordinates. The mutation table is already normalized; `source_row_id`
records its input row. The mutation belongs to `A_full`: A25V describes
residue 25 of that transcript’s protein, not residue 25 of every
isoform.

#### Construct the sequence and annotation objects

Create a FASTA and a new `SynIndividual` using the existing GTF.
Translation joins each transcript’s CDS in coding order. The output
below contains 70 characters for `A_full` and 40 for `A_skip`, including
a terminal `*`.

``` r

sequence_bases <- rep("N", 1700)
for (i in seq_len(nrow(coding_blocks))) {
  positions <- seq.int(coding_blocks$start[i], coding_blocks$end[i])
  sequence_bases[positions] <- strsplit(coding_blocks$dna[i], "")[[1]]
}
teaching_fasta <- tempfile(fileext = ".fa")
writeLines(c(">chr1", paste0(sequence_bases, collapse = "")), teaching_fasta)
sequence_model <- SynIndividual(
  genome_file = teaching_fasta, annotation_file = gtf_file,
  annotation_format = "gtf", id = "teaching-sequence"
)
sequence_model <- translate_protein(sequence_model, genes = "geneA")
translated <- as.character(protein_seq(sequence_model))
knitr::kable(data.frame(
  transcript = names(translated), residues_including_stop = nchar(translated),
  sequence_start = substr(unname(translated), 1, 12),
  terminal_character = substr(unname(translated), nchar(translated), nchar(translated))
))
```

|        | transcript | residues_including_stop | sequence_start | terminal_character |
|:-------|:-----------|------------------------:|:---------------|:-------------------|
| A_full | A_full     |                      70 | MAAAAAAAAAAA   | \*                 |
| A_skip | A_skip     |                      40 | MAAAAAAAAAAA   | \*                 |

Attach both annotations with `keytype = "transcript_id"`, and store the
normalized mutation table in memory. This restricts the mutation to its
own transcript. The `analysis` column identifies the domain model;
`model = "Pfam"` selects its records during projection.

``` r

teaching_domain_file <- tempfile(fileext = ".tsv")
write.table(domain_input, teaching_domain_file, sep = "\t", quote = FALSE,
            row.names = FALSE)
sequence_model <- add_annotation(sequence_model, SynProteinDomainAnnotation(
  name = "teaching_domains", domain_file = teaching_domain_file,
  keytype = "transcript_id"
))
sequence_model <- add_annotation(sequence_model, SynProteinMutationAnnotation(
  name = "teaching_mutations", mutation_file = "<in-memory>",
  keytype = "transcript_id", mutation_data = mutation_input, lazy = FALSE
))
domain_projection <- project_domains_to_genome(
  sequence_model, annotation = "teaching_domains", model = "Pfam",
  transcripts = "A_full"
)
mutation_projection <- project_mutations_to_genome(
  sequence_model, annotation = "teaching_mutations", genes = "geneA"
)
knitr::kable(domain_projection[, c("transcripts", "xmin", "xmax", "motif")])
```

| transcripts | xmin | xmax | motif  |
|:------------|-----:|-----:|:-------|
| A_full      |  143 |  160 | Domain |
| A_full      |  501 |  530 | Domain |

``` r

knitr::kable(mutation_projection[, c("transcripts", "xmin", "xmax", "mutation")])
```

| transcripts | xmin | xmax | mutation |
|:------------|-----:|-----:|:---------|
| A_full      |  513 |  515 | A25V     |

The domain splits into genomic intervals 143-160 and 501-530. A25V maps
to 513-515 in `A_full` and does not appear on `A_skip`. Using
`keytype = "gene_id"` would instead project the supplied residue number
onto every matching isoform. That does not establish an equivalent
protein consequence in each isoform; transcript-specific mutations
should use a transcript key.

#### Choose the geoms, scale, and theme

Draw neutral exon models, overlay the projected domain on the `A_full`
row, and add the mutation lollipop to its own transcript. A shared
`exon_height` makes the domain blocks as tall as the exons. Here
`y_offset = 2` places the single domain row over `A_full` in this
two-isoform layout; inspect row placement when changing transcripts or
domain models. The y-axis limits leave balanced space above and below
the transcript models and mutation marker. The x-axis includes space
before the first exon and beyond the terminal direction triangles, which
extend past the last exon.

``` r

projection_exon_height <- 0.8
projection_plot <- ggexon(sequence_model) +
  geom_exon(chr = "chr1", subset = c(80, 980), fill = "grey85",
            exon_height = projection_exon_height,
            colour = "grey50", transcript_arrow_length = 25) +
  geom_motif(aes(fill = motif), annotation = "teaching_domains", model = "Pfam",
             chr = "chr1", subset = c(80, 980), y_offset = 2,
             exon_height = projection_exon_height) +
  geom_aa_variant(aes(fill = mutation, label = mutation),
    annotation = "teaching_mutations", genes = "geneA",
    chr = "chr1", subset = c(80, 980), spread = FALSE,
    exon_height = projection_exon_height,
    colour = "grey20", label_angle = 0, label_size = 3) +
  scale_fill_manual(values = c(Domain = "#0072B2", A25V = "#D55E00"),
                    name = "Protein feature") +
  scale_x_continuous(limits = c(0, 1500), breaks = c(100, 500, 900, 1300),
                     labels = scales::label_number(big.mark = ","),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0.8, 6.6), expand = expansion(mult = 0)) +
  theme_ggexon_track(base_size = 10, show_legend = TRUE)
projection_plot
```

![Two centered Gene A transcript models with blank space on all sides
and visible right-pointing terminal triangles. Domain blocks match exon
height, and A25V appears only on the full
isoform.](ggexon-workflow_files/figure-html/workflow-projection-plot-1.png)

This x axis measures genomic base pairs. The preceding protein lollipop
example measures amino-acid positions. Projection uses annotation
structure; the plotted mutation is supplied teaching data, not a variant
inferred from the synthetic FASTA.

## 8. Aligned DNA and sequence differences

An alignment ribbon summarizes an interval but hides the bases that
differ. For a short verified pairwise alignment,
[`alignseq_data()`](https://dongyaoliu.github.io/ggexon/reference/alignseq_data.md)
attaches columns to genomic positions and
[`geom_alignseq()`](https://dongyaoliu.github.io/ggexon/reference/geom_alignseq.md)
draws the sequence detail above gene models. Reuse the annotation behind
`p_exon2` to keep the gene structure visible while inspecting nucleotide
differences.

### Present the input

Create two synthetic samples with the same teaching gene models and a
short alignment inside gene A’s first exon. The strings below are
supplied teaching data, not DNA inferred from the GTF. Each contains six
genomic bases, starting at position 131 on `chr1`. A dash is an
alignment gap and consumes no base.

``` r

dna <- c(sample_A = "AC-GTAC", sample_B = "ATCG-AC")
anchors <- data.frame(
  sequence = names(dna), start = 131L, strand = "+",
  track = names(dna), chr = "chr1", y = 4.6
)
knitr::kable(data.frame(sequence = names(dna), aligned_DNA = unname(dna)))
```

| sequence | aligned_DNA |
|:---------|:------------|
| sample_A | AC-GTAC     |
| sample_B | ATCG-AC     |

``` r

knitr::kable(anchors)
```

| sequence | start | strand | track    | chr  |   y |
|:---------|------:|:-------|:---------|:-----|----:|
| sample_A |   131 | \+     | sample_A | chr1 | 4.6 |
| sample_B |   131 | \+     | sample_B | chr1 | 4.6 |

### Construct and inspect the objects

Attach the same `gtf_file` used by `model` and `p_exon2` to each sample
in a `SynSpecies`. Each panel will read its gene models from that
object. Focus on the gene A window, which contains `A_full` and
`A_skip`. The anchor’s `y = 4.6` places the sequence detail just above
the upper transcript model.

``` r

dna_models <- SynSpecies(name = "Teaching DNA alignment")
for (sample_id in names(dna)) {
  dna_models <- add_individual(dna_models, SynIndividual(
    id = sample_id, annotation_file = gtf_file, annotation_format = "gtf"
  ))
}
individual_names(dna_models)
#> [1] "sample_A" "sample_B"
dna_columns <- alignseq_data(
  aligned = dna, anchors = anchors, reference = "sample_A",
  details = c(131, 136), flank = 0
)
knitr::kable(dna_columns[, c("track", "column", "base", "position", "event_type")])
```

| track    | column | base | position | event_type   |
|:---------|-------:|:-----|---------:|:-------------|
| sample_A |      1 | A    |      131 | match        |
| sample_A |      2 | C    |      132 | substitution |
| sample_A |      3 | \-   |       NA | insertion    |
| sample_A |      4 | G    |      133 | match        |
| sample_A |      5 | T    |      134 | deletion     |
| sample_A |      6 | A    |      135 | match        |
| sample_A |      7 | C    |      136 | match        |
| sample_B |      1 | A    |      131 | match        |
| sample_B |      2 | T    |      132 | substitution |
| sample_B |      3 | C    |      133 | insertion    |
| sample_B |      4 | G    |      134 | match        |
| sample_B |      5 | \-   |       NA | deletion     |
| sample_B |      6 | A    |      135 | match        |
| sample_B |      7 | C    |      136 | match        |

[`alignseq_data()`](https://dongyaoliu.github.io/ggexon/reference/alignseq_data.md)
returns an inspectable table. It classifies differences relative to
`sample_A`; it does not compute the alignment or infer evolutionary
direction. These are two illustrative samples, not sequence differences
inferred between the isoforms of one genome.

### Choose the geom, scales, and theme

Start with the gene models, then add the alignment as another layer.
Keep genomic intron lengths with `compress_introns = FALSE`. Reuse
`transcript_colours` from `p_exon2`: blue for `A_full` and orange for
`A_skip`. Nucleotide colours have their own palette and do not change
the transcript fill scale.

``` r

dna_gene_plot <- ggexon(dna_models) +
  geom_exon2(
    aes(fill = transcript_id), chr = "chr1", subset = c(80, 980),
    annotation_type = "exon", compress_introns = FALSE,
    exon_height = 0.8, arrow_width = 12, colour = "grey55"
  ) +
  scale_fill_manual(values = transcript_colours, name = "Transcript") +
  facet_genomics(vars(track), ncol = 1) +
  scale_x_continuous(limits = c(80, 980), breaks = c(100, 500, 900)) +
  scale_y_continuous(limits = c(1.2, 6.2), expand = expansion(mult = 0)) +
  theme_ggexon_track(base_size = 10, show_x_grid = FALSE, show_legend = TRUE)
```

The row view suits this seven-column alignment. Sequence letters have
equal column spacing within each callout; the gene models retain genomic
spacing. Each sequence block has one curved connector from its genomic
interval’s displayed midpoint to the centre of the block’s lower edge.

``` r

dna_gene_plot +
  geom_alignseq(data = dna_columns, mode = "rows", size = 3.5,
                sequence_position = "center", colour_scheme = "nucleotide")
```

![Two synthetic sample panels reuse the p_exon2 gene models. Each has a
centered nucleotide-coloured sequence block above the full and
exon-skipping transcripts, joined to the first-exon region by one
connector.](ggexon-workflow_files/figure-html/workflow-dna-plot-1.png)

Use `sequence_position = "left"`, `"center"`, or `"right"` to choose the
sequence block’s horizontal position. The default, `"auto"`, places it
near its genomic anchor and moves nearby blocks apart before stacking
them. Explicit positions keep blocks aligned and stack overlapping
details vertically. Wide blocks are kept inside the panel with room for
sequence names. This setting works in both modes and on reversed genomic
panels.

#### Highlight differences instead of nucleotide identity

Set `colour_scheme = "difference"` to use one colour for mismatches and
another for indels, with a neutral colour for the other alignment
columns. Here orange marks the mismatch, red marks both insertion and
deletion columns, and grey keeps aligned bases in the background. Both
samples highlight the same alignment columns, including the gap
character.

``` r

difference_palette <- c(aligned = "#E6E6E6", mismatch = "#E69F00", indel = "#D73027")
dna_difference_plot <- dna_gene_plot +
  geom_alignseq(
    data = dna_columns, mode = "rows", size = 3.5,
    sequence_position = "center", colour_scheme = "difference",
    difference_colours = difference_palette
  )
dna_difference_plot
```

![The same two sample gene models and DNA alignment, with mismatches
highlighted orange, insertion and deletion columns red, and aligned
columns grey. Each sequence block has one midpoint
connector.](ggexon-workflow_files/figure-html/workflow-dna-difference-colours-1.png)

Change the three named values in `difference_colours` to customize this
palette. Text switches between black and white to stay readable against
the background. With `background = FALSE`, only the mismatch and indel
letters receive highlight colours. Ambiguous IUPAC bases remain neutral
and are not classified as verified mismatches.

For larger regions, `mode = "markers"` draws differences and uses
`details` only for selected callouts. Match `track` names to existing
annotation panels when adding DNA detail to a comparison. The [AS3MT
example](https://dongyaoliu.github.io/ggexon/articles/as3mt-splice-variants-demo.md)
uses this approach at a real locus.

## 9. Equal gene slots for a presence and order comparison

Base-pair spacing can obscure corresponding gene families across loci. A
shared slot template makes occupied and empty positions visible,
changing the meaning of the x axis to comparison slots.

### Present the input

These synthetic clusters have genes assigned to three slots. Cluster B
has no row for slot 2; this input omission alone does not establish gene
loss.

``` r

slot_input <- data.frame(
  cluster = c("A", "A", "A", "B", "B"),
  position = c(100, 300, 1000, 500, 900),
  slot = c("s1", "s2", "s3", "s1", "s3"),
  strand = c("+", "-", "+", "-", "+")
)
knitr::kable(slot_input)
```

| cluster | position | slot | strand |
|:--------|---------:|:-----|:-------|
| A       |      100 | s1   | \+     |
| A       |      300 | s2   | \-     |
| A       |     1000 | s3   | \+     |
| B       |      500 | s1   | \-     |
| B       |      900 | s3   | \+     |

### Prepare the plotting data

Retain genomic positions in `x`. The strip scale maps these to the
explicit template rather than requiring us to overwrite source
coordinates.

``` r

slot_data <- transform(slot_input, x = position, y = 1, track = cluster,
                       gene_key = slot, genomic_x = position)
slot_order <- c("s1", "s2", "s3")
```

### Choose the geom, scales, facet, and theme

[`geom_genebox()`](https://dongyaoliu.github.io/ggexon/reference/geom_genebox.md)
uses equal-size squares with strand arrows.
[`strip_scale_x()`](https://dongyaoliu.github.io/ggexon/reference/strip_scale_x.md)
reserves every slot, including unoccupied ones.
[`scale_x_facet_labels()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_facet_labels.md)
names each facet’s slots, and
[`coord_fixed()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html)
keeps boxes square. The clean theme retains ordinary axis labels.

``` r

ggexon(slot_data) +
  geom_genebox(data = slot_data, aes(fill = slot), box_size = 0.65) +
  facet_grid(. ~ cluster) +
  strip_scale_x(slot_order = slot_order, guide = "none") +
  scale_x_facet_labels(
    cluster, labels = list(A = c("A1", "A2", "A3"), B = c("B1", "B2", "B3"))
  ) +
  scale_fill_manual(values = c(s1 = "#0072B2", s2 = "#D55E00", s3 = "#009E73")) +
  coord_fixed(ratio = 1) +
  theme_ggexon_clean(base_size = 10) +
  theme(axis.title = element_blank(), axis.text.y = element_blank(),
        panel.grid = element_blank(), legend.position = "none")
```

![Two three-slot gene clusters. Cluster B has an empty middle slot;
occupied slots contain strand
arrows.](ggexon-workflow_files/figure-html/workflow-slots-1.png)

The x axis now names supplied slots, not base pairs. With Syn-backed
data, `geom_genebox(anchor = "start")`, `"middle"`, or `"end"` chooses
the coding anchor before scaling.

## 10. Align genomic tracks to a tree

A phylogenetic ordering can make a multi-genome comparison easier to
read. ggexon aligns one spanning rectangular tree to genomic rows
without manual positioning of separate plots.

### Present the input

Create two small synthetic gene tracks and supply a synthetic Newick
tree. The intervals and branch lengths are illustrative. This optional
example requires `ape` and `ggtree`.

``` r

genes <- data.frame(
  track = rep(c("reference", "query"), each = 2),
  xmin = c(100, 400, 1100, 1600), xmax = c(250, 600, 1300, 1750),
  y = 1, strand = c("+", "-", "+", "-"), gene = rep(c("A", "B"), 2)
)
tree_text <- "(reference:0.1,query:0.2);"
tree_text
#> [1] "(reference:0.1,query:0.2);"
knitr::kable(genes)
```

| track     | xmin | xmax |   y | strand | gene |
|:----------|-----:|-----:|----:|:-------|:-----|
| reference |  100 |  250 |   1 | \+     | A    |
| reference |  400 |  600 |   1 | \-     | B    |
| query     | 1100 | 1300 |   1 | \+     | A    |
| query     | 1600 | 1750 |   1 | \-     | B    |

### Prepare the objects

Tree-tip names must match track IDs, or be mapped with the `individual`
argument to
[`geom_genomic_tree()`](https://dongyaoliu.github.io/ggexon/reference/geom_genomic_tree.md).

``` r

tree_available <- requireNamespace("ape", quietly = TRUE) &&
  requireNamespace("ggtree", quietly = TRUE)
if (tree_available) {
  teaching_tree <- ape::read.tree(text = tree_text)
  tree_tracks <- transform(genes, track = as.character(track))
}
```

### Choose the geom, scales, facet, and theme

[`geom_genomic_tree()`](https://dongyaoliu.github.io/ggexon/reference/geom_genomic_tree.md)
attaches the tree;
[`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md)
orders and aligns the panels. Genomic x ranges and tree branch lengths
have separate scales and meanings. This compact view hides the
branch-length ruler; branch lengths remain specified in the Newick
input.

``` r

if (tree_available) {
  ggexon(tree_tracks) +
    geom_genetag(aes(fill = gene), show_label = FALSE) +
    geom_genomic_tree(tree = teaching_tree, tree_width = grid::unit(1.2, "in")) +
    facet_genomictree(scales = "free_x", show_tree_x_axis = FALSE) +
    scale_fill_manual(values = c(A = "#0072B2", B = "#D55E00")) +
    scale_x_continuous(labels = scales::label_number()) +
    theme_ggexon_genomictree(base_size = 10)
} else {
  cat("Install ape and ggtree to render this optional example.")
}
```

![A synthetic two-tip tree aligned with reference and query gene
tracks.](ggexon-workflow_files/figure-html/workflow-tree-plot-1.png)

For repeated multi-locus comparisons, store comparable windows in a
`SynLocusSet`, attach it with
[`add_locus_set()`](https://dongyaoliu.github.io/ggexon/reference/add_locus_set.md),
and use
[`use_locus_grid()`](https://dongyaoliu.github.io/ggexon/reference/use_locus_grid.md).
This records the chosen loci alongside the individuals.

## 11. Correct an annotation and compare the result

A gene-model correction is easier to review when it remains attached to
the annotation and you can still plot the original. A patch records the
target, replacement features, and a name for the change.

### Present the correction

Shorten gene A’s middle exon from 590 to 560 in this synthetic exercise.
Supply the complete corrected gene model, including its unchanged
children.

``` r

corrected_ranges <- query_features(model, genes = "geneA", feature_type = NULL, all = TRUE)
change <- as.character(S4Vectors::mcols(corrected_ranges)$type) %in% c("exon", "CDS") &
  BiocGenerics::start(corrected_ranges) == 501
BiocGenerics::end(corrected_ranges)[change] <- 560L
knitr::kable(as.data.frame(corrected_ranges)[,
  c("start", "end", "type", "gene_id", "transcript_id")])
```

| start | end | type       | gene_id | transcript_id |
|------:|----:|:-----------|:--------|:--------------|
|   101 | 960 | gene       | geneA   | NA            |
|   101 | 960 | transcript | geneA   | A_full        |
|   101 | 160 | exon       | geneA   | A_full        |
|   501 | 560 | exon       | geneA   | A_full        |
|   901 | 960 | exon       | geneA   | A_full        |
|   101 | 160 | CDS        | geneA   | A_full        |
|   501 | 560 | CDS        | geneA   | A_full        |
|   901 | 960 | CDS        | geneA   | A_full        |
|   101 | 960 | transcript | geneA   | A_skip        |
|   101 | 160 | exon       | geneA   | A_skip        |
|   901 | 960 | exon       | geneA   | A_skip        |
|   101 | 160 | CDS        | geneA   | A_skip        |
|   901 | 960 | CDS        | geneA   | A_skip        |

### Construct and apply the patch

``` r

correction <- SynAnnotationPatch(
  name = "shorter-middle-exon", target_ids = "geneA", mode = "replace",
  patch_data = corrected_ranges
)
corrected_model <- patch_annotation(model, correction)
names(list_patches(corrected_model))
#> [1] "shorter-middle-exon"
```

[`patch_annotation_from_gff()`](https://dongyaoliu.github.io/ggexon/reference/patch_annotation_from_gff.md)
imports corrections from a small GFF. Replacing models clears stale
sequence caches; recompute derived sequences afterwards.

[`reset_patches()`](https://dongyaoliu.github.io/ggexon/reference/reset_patches.md)
clears all recorded patches and restores the base feature annotation. A
removal is also recorded as a patch, so it can be inspected and reset in
the same way. These operations return updated objects; `model` remains
available for the comparison below.

``` r

restored_model <- reset_patches(corrected_model)
length(list_patches(restored_model))
#> [1] 0

without_gene_b <- patch_annotation(
  model, patch = NULL, target_ids = "geneB", mode = "remove",
  name = "remove-gene-b"
)
names(list_patches(without_gene_b))
#> [1] "remove-gene-b"
unique(as.character(S4Vectors::mcols(annotation_data(without_gene_b))$gene_id))
#> [1] "geneA"
```

### Choose the geom, scales, facet, and theme

Retain the same base-pair scale before and after to compare the
boundary. Use uncompressed
[`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md)
models so the direction arrows sit at the terminal exons in this compact
comparison.

``` r

before <- load_annotation(SynIndividual(
  annotation_file = gtf_file, annotation_format = "gtf", id = "before"
))
after <- load_annotation(SynIndividual(
  annotation_file = gtf_file, annotation_format = "gtf", id = "after"
))
after <- patch_annotation(after, correction)
curation <- add_individual(SynSpecies("Annotation curation"), before)
curation <- add_individual(curation, after)
ggexon(curation) +
  geom_exon2(aes(fill = transcript_id), chr = "chr1", subset = c(80, 980),
             compress_introns = FALSE, arrow_width = 12) +
  facet_genomics(vars(track), ncol = 1, scales = "fixed", strip.position = "left") +
  scale_fill_manual(values = transcript_colours) +
  scale_panel_annotation("fixed_y") +
  scale_x_continuous(breaks = c(100, 500, 900)) +
  theme_ggexon_track(base_size = 10) +
  theme_ggexon_side_strips("left", base_size = 10)
```

![Before and after synthetic annotation panels showing a shorter middle
exon following a
patch.](ggexon-workflow_files/figure-html/workflow-patch-plot-1.png)

## 12. Query a VCF layer and mark variant positions

A VCF records allele changes at genomic positions. The feature
annotation and variant layer remain separate: attaching a VCF does not
change the gene model. This short example reuses `model` from the first
example.

### Present the input

Create three synthetic single-nucleotide variants on the same `chr1` as
the teaching GTF. VCF `POS` uses one-based coordinates; these example
alleles are teaching labels and have not been checked against a
reference FASTA.

``` r

vcf_lines <- c(
  "##fileformat=VCFv4.2",
  "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
  "chr1\t125\tv1\tA\tG\t60\tPASS\t.",
  "chr1\t535\tv2\tC\tT\t80\tPASS\t.",
  "chr1\t925\tv3\tG\tA\t50\tPASS\t."
)
cat(vcf_lines, sep = "\n")
#> ##fileformat=VCFv4.2
#> #CHROM   POS ID  REF ALT QUAL    FILTER  INFO
#> chr1 125 v1  A   G   60  PASS    .
#> chr1 535 v2  C   T   80  PASS    .
#> chr1 925 v3  G   A   50  PASS    .
vcf_file <- tempfile(fileext = ".vcf")
writeLines(vcf_lines, vcf_file)
```

### Construct and inspect the object

Attach a named `SynVCFAnnotation`, retrieve it, and query the displayed
interval. Plain-text VCF is sufficient for this small example. Large
BGZF VCFs can use a tabix index for regional access. BCF parsing is
unavailable.

``` r

variant_model <- add_annotation(
  model, SynVCFAnnotation(name = "variants", vcf_file = vcf_file)
)
variant_rows <- as.data.frame(query_variants(
  get_annotation(variant_model, "variants"),
  chr = "chr1", start = 80, end = 980
))
knitr::kable(variant_rows[, c("CHROM", "POS", "ID", "REF", "ALT")])
```

| CHROM | POS | ID  | REF | ALT |
|:------|----:|:----|:----|:----|
| chr1  | 125 | v1  | A   | G   |
| chr1  | 535 | v2  | C   | T   |
| chr1  | 925 | v3  | G   | A   |

``` r

variant_rows$allele <- paste0(variant_rows$REF, ">", variant_rows$ALT)
```

### Choose the geoms, scales, and theme

Use the stored feature annotation for exon models and an ordinary
[`geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html)
for the queried variant positions. Both use the same genomic x
coordinates. The lines mark sites across transcript rows; they do not
represent frequencies, coverage, or allele-specific transcript
structures.

``` r

vcf_plot <- ggexon(variant_model) +
  geom_exon(chr = "chr1", subset = c(80, 980), fill = "grey65",
            transcript_arrow_length = 25) +
  geom_vline(
    data = variant_rows, aes(xintercept = POS, colour = allele),
    linewidth = 0.6, linetype = "dashed", inherit.aes = FALSE
  ) +
  scale_colour_manual(values = c("A>G" = "#0072B2", "C>T" = "#D55E00", "G>A" = "#009E73"),
                      name = "REF > ALT") +
  scale_x_continuous(breaks = c(100, 500, 900), labels = scales::label_comma()) +
  theme_ggexon_track(base_size = 10, show_legend = TRUE)
vcf_plot
```

![Three synthetic variant sites marked as vertical lines across two gene
A transcript
models.](ggexon-workflow_files/figure-html/workflow-vcf-plot-1.png)

## Choose a view for your question

| Question | Object or preparation | Geometry | Scale and theme |
|----|----|----|----|
| Which exons and isoforms are annotated? | `SynIndividual` + features | [`geom_exon()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon.md) | Genomic bp; track theme |
| How do exon structures compare across long introns? | Feature layer | [`geom_exon2()`](https://dongyaoliu.github.io/ggexon/reference/geom_exon2.md) | [`scale_x_ggexon_genomic()`](https://dongyaoliu.github.io/ggexon/reference/scale_x_ggexon_genomic.md); piecewise guide |
| What is the order and direction of genes? | Feature layer + plot labels | [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md), [`geom_genelabel()`](https://dongyaoliu.github.io/ggexon/reference/geom_genelabel.md) | Genomic bp or strip scale; track theme |
| Which intervals align between genomes? | `SynSpecies` + `SynPairAlignment` | [`geom_nuclink()`](https://dongyaoliu.github.io/ggexon/reference/geom_nuclink.md) | Independent genomic x; pairwise theme |
| How do gene spans relate to aligned blocks? | `SynSpecies` + stored alignment and panel windows | [`geom_genetag()`](https://dongyaoliu.github.io/ggexon/reference/geom_genetag.md), [`geom_synteny_link()`](https://dongyaoliu.github.io/ggexon/reference/geom_synteny_link.md) | Independent genomic x; pairwise theme |
| How does signal vary across a locus? | `SynBigWigAnnotation` | [`geom_coverage()`](https://dongyaoliu.github.io/ggexon/reference/geom_coverage.md) | Genomic x; explicit coverage y policy |
| Where are protein domains and mutations? | Mutation and domain tables/layers | [`geom_protein_lollipop()`](https://dongyaoliu.github.io/ggexon/reference/geom_protein_lollipop.md), [`geom_motif()`](https://dongyaoliu.github.io/ggexon/reference/geom_motif.md), [`geom_aa_variant()`](https://dongyaoliu.github.io/ggexon/reference/geom_aa_variant.md) | Protein residues or projected genomic bp |
| Where are genomic variants recorded? | `SynVCFAnnotation` + [`query_variants()`](https://dongyaoliu.github.io/ggexon/reference/query_variants.md) | Exons + ordinary [`geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html) | Genomic bp; track theme |
| Which bases differ in an existing alignment? | [`alignseq_data()`](https://dongyaoliu.github.io/ggexon/reference/alignseq_data.md) | [`geom_alignseq()`](https://dongyaoliu.github.io/ggexon/reference/geom_alignseq.md) | Anchored genomic coordinates; track theme |
| Which comparison slots are occupied? | Gene-slot table | [`geom_genebox()`](https://dongyaoliu.github.io/ggexon/reference/geom_genebox.md) | [`strip_scale_x()`](https://dongyaoliu.github.io/ggexon/reference/strip_scale_x.md), facet labels; clean theme |
| How do loci relate to a supplied tree? | Matching tracks and tree | [`geom_genomic_tree()`](https://dongyaoliu.github.io/ggexon/reference/geom_genomic_tree.md) | [`facet_genomictree()`](https://dongyaoliu.github.io/ggexon/reference/facet_genomictree.md); genomic-tree theme |

[`theme_ggexon_base()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_base.md)
is the common background-free foundation;
[`theme_ggexon_clean()`](https://dongyaoliu.github.io/ggexon/reference/theme_ggexon_clean.md)
keeps ordinary axes and grids. Choose the track theme when y only
positions annotations; retain a numeric y axis for quantitative
summaries. Side-strip styling supplements a theme and must match the
facet’s `strip.position`. Add ordinary
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
overrides after the complete theme.

Export a saved plot object with explicit dimensions and inspect the
result. For example,
`ggsave("exon-models.pdf", plot = p_exons, width = 9, height = 3.5)`
exports the first model view.

The [function
reference](https://dongyaoliu.github.io/ggexon/reference/index.md)
describes individual classes, geoms, facets, and scales. The four
[biological
tutorials](https://dongyaoliu.github.io/ggexon/articles/index.md) expand
these recipes with documented sources.
