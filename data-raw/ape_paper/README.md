# Ape immune-locus atlas

Real-data ggexon views combining the IG/TR loci in Extended Data Fig. 2 and MHC annotations in Extended Data Fig. 3 of [Yoo et al., Complete sequencing of ape genomes (2025)](https://www.nature.com/articles/s41586-025-08816-3). These are newly assembled views, not pixel reproductions of the published figures.

The active project contains only immune-locus examples. Changes and outputs live in the isolated worktree `/Users/liudongyao/.codex/worktrees/ape-integrated-annotations/ggexon`, branch `codex/ape-integrated-annotations`. The primary checkout is preserved.

## Continuous MHC revision (19 October 2026)

The current MHC result is `results/mhc_continuous/figures/mhc_continuous.pdf`. It combines Extended Data Fig. 3a/c on one continuous native-coordinate MHC track per haplotype, with a common bp scale, native genomic coordinate ticks, left-side track labels, the border-free ggexon theme, real nucleotide ribbons and context genes on both ape haplotypes. Released annotations and same-species Liftoff projections are explicitly distinguished. MHC genes have small native-span arrows beneath enlarged labelled heads. Native arrows use the exact source ortholog colors where defined; enlarged pseudogene heads remain grey with an asterisk, and context genes remain grey. Pseudogene-only keys without a separate source color retain source grey in the native layer. Variable-region annotations and class labels sit below the genes; there are no tree panels.

See `results/mhc_continuous/README.md` for reproduction, the reconstructed 5.309-Mb human extent, source coordinate uncertainty and the six alternate haplotypes with inferred context projections and nine unresolved complete models after five alternative-isoform recoveries. The earlier separate class-I/class-II atlas below is retained as an earlier alternative view; it is not the continuous revision.

## Figure set

- `results/immune_atlas/immune_combined.pdf`: eight genomic facets, each with the same nine species/haplotype tracks. IGH, IGK, IGL, shared TRA/TRD, TRB, TRG, MHC class I and MHC class II. TRA and TRD share a physical region and are not counted twice.
- Individual vector PDFs and PNG previews in that directory: `IGH`, `IGK`, `IGL`, `TRA`, `TRB`, `TRG`, `MHC_I`, `MHC_II`. The `TRA` file is the combined TRA/TRD region.
- `MHC_I_all_species` and `MHC_II_all_species`: expanded standalone MHC views with chimpanzee and siamang, thirteen tracks in total.
- `functional_gene_counts.pdf`: companion count facets integrating the two source tables.
- `results/figures/mhc.pdf`: detailed DR-region view with actual human/chimpanzee nucleotide alignments, curated functionality intervals and CpG methylation below gene models.

The combined atlas uses the nine tracks shared by the published IG/TR plot data: bonobo h1/h2, human CHM13, gorilla h1/h2, Bornean orangutan h1/h2 and Sumatran orangutan h1/h2. Their order follows the eight published adjacent alignment pairs. MHC standalone plots retain all available source species. IG/TR source tracks do not provide chimpanzee/siamang locus alignments.

## Reading the atlas

Gene spans and small centre markers encode V, D, J or unclassified segments. MHC spans encode functional genes versus pseudogenes. The centre markers aid visibility of short genes without changing interval endpoints. In each IG/TR annotation panel, the three lanes beneath the genes show ORF intervals, source species-specific calls, then source SV blocks. The summary below those lanes gives functional/ORF V counts and species-specific calls (SS), followed by the number with unknown specificity (+?). Human functional status is unknown in this IG/TR source and is shown as `?`.

Only nucleotide links occupy separate panels. All other annotations belong to their genome annotation panel below the gene spans. MHC overview views contain gene organization and curated functionality; they do not invent nucleotide links from shared gene names. The detailed DR view supplies actual MHC nucleotide links and methylation.

IG/TR coordinates are extracted-locus-relative kilobases, with the published locus orientation applied to genes, links and SV blocks alike. The MHC overview shows kilobases from the first curated gene for each track: its window is an observed gene span, not a complete independently established MHC boundary. Axes have an export-local correction for ggexon's repeated reference-coordinate tick labels. Different tracks can have different axis extents; the figures do not establish copy-number-normalized locus expansion.

Source SV colors have no published categorical key, so blocks are drawn uniformly and are called SV blocks, not segmental duplications. ORF is retained as the paper's status, not relabelled pseudogene. Missing calls are not biological absence.

## Data and provenance

`fetch_igtr.py` downloads pinned public IG/TR repository files, compact locus FASTAs and Supplementary Data 1. Preferred `results/igtr/paper_genes.tsv` has 3,738 source-backed rows: 3,346 final ape annotations and 392 human repository/BED annotations. `paper_counts.tsv` preserves exact family/status counts. Human delta annotations are supplemented from the repository's NCBI BED, reclassifying existing overlapping features rather than duplicating them. Ape TRDD calls are not supplied. See `results/igtr/SCHEMA.md` for coordinate evidence, annotation-version differences, sequence checks and upstream inconsistencies. Published source plots and final supplementary annotations are distinct annotation versions; source species-specific flags are transferred only with positional/type evidence. Unknown flags remain unknown.

`fetch_mhc_full.py` prepares `results/mhc_full/genes_ed3_scope.tsv`: 269 curated gene records across thirteen tracks, with 191 records in the common nine-track atlas. Human ED3 scope has eight class I records (six functional, two pseudogenes) and thirteen class II records. Human spans are transcript unions from CHM13; ape spans are curated source coding genes. These differ in ascertainment and do not support unqualified absence claims. `source_metadata/mhc_full/sources.json` records original URLs, SHA256 values and coordinate conventions. The broader `genes.tsv` retains 285 records.

The quoted paper-wide average counts are context, not input data. Plotted counts are recomputed from the exact source annotation rows and may differ with source version, cohort or functional-status definition. No counts were adjusted to reproduce the prose averages.

Raw source files and checksum manifests are retained under `source_metadata/igtr/` and `source_metadata/mhc_full/`. The whole-paper supplementary workbook is shared source material for MHC. Full genomes are not downloaded.

## Reproduce and verify

From this worktree root:

```sh
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/fetch_igtr.py
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/fetch_mhc_full.py
Rscript data-raw/ape_paper/render_immune_atlas.R
Rscript data-raw/ape_paper/render_immune_counts.R
Rscript data-raw/ape_paper/render_figures.R mhc
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/verify_inputs.py
Rscript data-raw/ape_paper/test-integrated-layout.R
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/verify_immune_atlas.py
```

The atlas renderer checks locus containment, published pair adjacency, exact gene-row preservation, lane ownership and below-gene placement. Its `.layout-qc.tsv`, `.lane-qc.tsv`, `.plotted-genes.tsv` and `.summaries.tsv` files provide export evidence. The detailed MHC view additionally checks raw signal scores and native endpoints. No package-core edits are required.

Annotation symbols: gene-centre dots share the actual gene-span lane; grey crosses mark ORF calls, red triangles mark source species-specific calls, and grey bars mark SV blocks. All annotation intervals retain their actual coordinates; point symbols are visibility aids. Functional count heatmap cells show exact numeric counts, with quantile bands used only for color. `NA` means the functional classification was not available, not zero genes. Count matrix identifiers match the atlas: HSA human, PPA bonobo, GGO gorilla, PAB Sumatran orangutan, PPY Bornean orangutan; suffix 1/2 identifies source haplotype.
