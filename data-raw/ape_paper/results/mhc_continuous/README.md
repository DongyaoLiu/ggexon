# Continuous native-coordinate MHC comparison

This figure combines the gene identity/status information of Extended Data Fig. 3a/b with the continuous region/alignment design of Fig. 3c in [Yoo et al. (2025)](https://www.nature.com/articles/s41586-025-08816-3/figures/11). It is a source-backed reconstruction, not an exact reproduction of the original plotted alignment.

## Outputs

- `figures/mhc_continuous.pdf`: 180 × 818.81 mm vector figure with 13 native annotation tracks and 12 nucleotide-link panels.
- `figures/mhc_continuous.png`: preview.
- `windows.tsv`: native chromosome boundaries, lengths and orientations.
- `links.tsv`: regenerated adjacent nucleotide blocks, with original PAF/CIGAR row references.
- `gene_models/`: original bounded source model/exon queries, schemas, hashes and manifest.
- `gene_models_with_projected_context/`: original 956 strict context projections and unchanged released records.
- `gene_models_with_recovered_context/`: final merged input with five additional shorter alternatives; all 14-case audit, RNA QC, native spans, provenance and independent checks.
- `figures/representative-models.tsv` and `plotted-gene-arrows.tsv`: representative source models and grey context gene intervals drawn with `geom_gene()`. `source-representative-exons.tsv` retains supporting exon records for provenance; exon glyphs are not drawn.
- `figures/plotted-mhc-genes.tsv`: 285 curated MHC gene records with status and unmodified native distances.
- `figures/plotted-native-mhc-arrows.tsv`: one small curated-span arrow per MHC gene, with native BED0 bounds, display strand, actual built fill and source color basis.
- `figures/mhc-anchors.tsv`: their true midpoint anchors, displayed strands, labels and original source colors.
- `figures/mhc-lollipop-layout.tsv`: final physical head/anchor positions, panel dimensions, rows and measured label widths, separately recorded while drawing the PDF and PNG.
- `discordant_regions.tsv`: exact human source intervals and separately labelled chain-supported projections.
- `figures/native-axis-qc.tsv`, `figures/*qc.tsv` and `verification.json`: source/geometry/rendering checks.

## Continuous geometry

Every track uses the same bp-to-horizontal-distance conversion. There is one native origin for each extracted chromosome interval, covering MHC class I, the intervening region and class II continuously. Source sequence lengths range from 4.79 Mb to 5.80 Mb. No continuous backbone is drawn through the gene annotation. Regions retain their native distances and are neither stretched to equal lengths nor concatenated across gene clusters. Siamang is reverse-complemented for comparable gene order; all lengths are preserved.

Native chromosome positions remain in all source tables. The display coordinate is translation from the interval start for plus orientation, or reflection from its end for minus orientation. Each axis shows one-based genomic coordinates in bp for its own native assembly chromosome. Siamang coordinates decrease from left to right because its region is reverse-oriented. The bp-to-horizontal-distance scale remains identical across tracks. The figure uses `theme_ggexon_track()` and left-side `theme_ggexon_side_strips()`, with no panel or strip box outlines. The 285 MHC genes, including pseudogenes, appear as enlarged directional heads drawn with `geom_gene_lollipop()`. Each label is inside its head. Heads use six shared physical rows, with an explicit `mhc-head-row-template.tsv` mapping each of the 41 source gene keys to its row across every haplotype. Functional and pseudogene records of the same source key share a row. Distinct duplicated copies remain separate heads, placed side by side on that row with their own true anchors. Read occupied slots top to bottom within each column, then left to right across columns; a new column starts when the fixed rows would otherwise overlap or run upwards. Empty slots stay blank and do not establish gene absence. The CHM13 rows supply the reference template; four ape-only source keys have explicit display assignments without merging identities or inferring new orthology. Their horizontal movement remains bounded at 25 mm. Coloured leaders rise from thin exact-midpoint ticks, fan through a clear physical band, then follow separate lanes beside each column to enter the left edge of their own head. The stems use a darker shade of the ortholog fill with thin white halos; pseudogene stems are charcoal. Leaders do not cross or pass behind other heads. One common head width fits the longest label, including padding and the directional tip; 4 mm head heights and 0.8 mm clear gaps beyond the outlines remain physical dimensions on both output devices. Rounded outlines are kept within the panel, including half-stroke clearance at the upper and side edges. Heads occupy collision-free rows above the anchors within a shared 8-unit headroom. Grey context genes retain their native genomic scale in the row immediately below the ticks, and VR1–VR3 share one lane beneath that row. All tracks reserve the same six head rows with common physical offsets above their genomic anchors. This increases figure height while retaining the existing font size, head dimensions and source bp geometry. Each of the 285 curated MHC records also has a small `geom_gene()` arrow at its true genomic span in the native gene row immediately beneath the stem. These arrows retain actual widths and strands without minimum-width expansion, transcript backbones or extra labels. They draw over grey context so their defined ortholog colors remain visible. The enlarged heads retain their previous size, positions, rows, status colors and labels. The draw-time layout table independently checks every head for overlap, label fit, panel bounds, strand, unchanged genomic anchors and column reading order, shared source-key rows and physical row offsets across haplotypes, plus every leader segment for proper crossings and intersections with other head outlines. `annotation-height-qc.tsv` and `figure-dimensions.tsv` record the final physical dimensions. The previous export is retained in `figures/before-tracing-polish/`. The human and gorilla class II detail PNGs are native-scale crops rendered from the full vector PDF for eye inspection; they are not extra zoom panels or rescaled data.

The pre-restoration export and renderer are archived in `figures/before-native-mhc-arrows/`. The updated verifier requires all 285 native arrows across 13 haplotypes, exact source spans, once-reflected strands, source ortholog/pseudogene colors and actual built fills. It also requires an identical full head-and-stem layout table, grey context table, native axes, common bp scale, panel heights and figure dimensions compared with that archive. Native arrow colors bypass legend scale training, preserving the head legend and figure geometry.

## Bounds and source limitations

The original figure caption states 5.31 Mb but the public materials examined do not specify its exact extraction endpoints. The methods paragraph describing GABBR1–KIFC1 is a narrower manual MHC analysis and includes malformed printed coordinates. The actual panel c has ZNF311 and GRM4 landmarks across a broader region, and includes HG002 h1/h2 in addition to the 13 tracks selected here.

The explicit reconstruction rule takes the union of the published three variable-region intervals and CHM13 RefSeq v5.2 ZNF311–GRM4 landmark gene spans. This gives CHM13 chr6 BED0[28,669,878,33,979,214): 5,309,336 bp, which rounds to 5.31 Mb. This precision describes our documented extraction, not proof of the original authors' cut points. Ape windows are the native union of browser chain blocks supported within that reference interval. Their endpoints therefore can differ in boundary-alignment coverage. `manifest.json` records the rule and sources.

Supplementary Table XI.59 explicitly identifies CHM13 chr6 and reports:

| Region | Source start | Source end | Source size |
|---|---:|---:|---:|
| VR1 |28,669,878|29,944,878|1,275,000|
| VR2 |30,924,878|31,319,878|395,000|
| VR3 |32,274,878|32,934,878|660,000|

The worksheet does not declare indexing. We operationally treat these as half-open intervals to preserve its stated Size=End−Start and retain the original values. The uncertainty is one base, not an inferred new regional boundary. VR1, VR2 and VR3 share one horizontal annotation lane below the genomic anchors in every track; class I/II spans sit below that lane. On human, bars are the published intervals. On apes, bars show only the portions projected through actual human-to-ape ungapped chain blocks; alignment gaps remain missing. They describe shared comparative regions and are not independent ape-specific discordance measurements. `discordant_projection_qc.tsv` gives mapped fractions; in siamang, some regions have only approximately 54–84% reference coverage. No topology categories, coalescent trees or tree panels are drawn.

## Models and colors

Exact upstream ortholog RGB values from `marbl/T2T-Browser/src/MHC/colors.txt` override the default plotting palette as requested. Enlarged pseudogene heads are grey with an asterisk; context genes are neutral grey. Small native MHC arrows use the source ortholog color whenever one is defined, including pseudogenes with that ortholog identity. The palette supplies no separate color for 18 pseudogene-only source keys (28 records); those native arrows retain the original source pseudogene grey. No new orthology or colors are inferred. Asterisks and functional statuses come from source annotation, not experimental validation. Class I/II underlines show the observed spans of curated class-associated genes, not separately reset coordinate panels.

All available source genes in the window are represented; for overlapping transcripts with the same gene symbol and strand at a locus, the model with the longest total exonic length is selected for readability. This does not display every transcript isoform. Complete non-MHC context intervals are drawn as grey arrows using `geom_gene()`; exon blocks are not displayed. Source transcript models supported by curated MHC records are excluded from the grey context layer to avoid drawing a second, differently bounded transcript representation. The separate native MHC layer uses the curated gene span for every haplotype, including those without released general transcript models. Additional duplicate exclusion requires the same ortholog name, track, strand and an overlapping curated interval; unmatched source models remain grey context. `excluded-native-mhc-models.tsv` records the excluded source rows, reasons and curated references. Native exon records are retained only as source evidence and for representative-transcript selection. Released models are CAT/RefSeq consensus v3 in primary apes and CHM13 RefSeq v5.2 in human; alternate context models are explicitly labeled same-species Liftoff projections below. Position+strand and matching names where available associate curated MHC genes with source exon models. Every curated MHC record is drawn once as a small native-scale arrow from its curated boundaries and once with `geom_gene_lollipop()` at the midpoint of the same source span. Pseudogene asterisks remain inside the enlarged heads. The source span is encoded by the small native arrow and retained in the audit table; the fixed-size enlarged head supplies a readable label and direction.

Released consensus tracks cover seven of the 13 haplotypes: HSAh1, PTRh1, PPAh2, GGOh1, PABh1, PPYh1 and SSYh1. Official release notes specify primary-only annotation. A live search of 48 browser BigBeds plus the six exact NCBI alternate assemblies found no released consensus models on the other six MHC chromosomes. Exact URLs and chromosome-dictionary evidence are in `source_metadata/mhc_context_search/`. GenArk has anonymous Augustus predictions and permissive cross-species RefSeq RNA alignments, but the former are incomplete in orangutans and the latter include unsuitable non-primate matches, so neither is substituted as a complete gene annotation.

The six previously empty context layers now contain **explicitly inferred same-species Liftoff projections** from the annotated parental haplotype. These use the exact regional FASTAs already used for the nucleotide ribbons. One existing representative per source context locus is transferred, excluding all recognized MHC-symbol records even when they escaped the original positional matching. No extra-copy search, MHC-head transfer, CDS transfer or new functional-status inference is performed. Both exon coverage and sequence identity must be at least 95%; partial/low-identity mappings are excluded. All native coordinates and strands are reconstructed after mapping, including reversed siamang, without rescaling. `gene_models_with_projected_context/` contains raw GFFs, source assignments, native model/exon tables, command/input hashes, exact tool provenance and the full failure audit. Original released model/exon fields are preserved unchanged.

| Alternate track | Source track | Eligible context loci | Accepted projections | Unresolved |
|---|---|---:|---:|---:|
| SSYh2 | SSYh1 | 151 | 150 | 1 |
| PABh2 | PABh1 | 164 | 158 | 6 |
| PPYh2 | PPYh1 | 163 | 161 | 2 |
| GGOh2 | GGOh1 | 164 | 161 | 3 |
| PPAh1 | PPAh2 | 164 | 162 | 2 |
| PTRh2 | PTRh1 | 164 | 164 | 0 |

Thus both haplotypes now have a general context layer. Strips explicitly distinguish `context annotated` from `context projected`. The initial 956 added models are visualization context, **not a complete validated alternate-haplotype gene catalog**. At the initial transfer stage fourteen source loci were unresolved. The subsequent recovery audit below adds five supported shorter alternatives and leaves nine complete models unresolved; none is called absent. A primary-to-alternate transfer cannot discover alternate-specific context genes missing from the source. Unresolved loci include MUC21/MUC22/MUCL3, several olfactory receptors, LY6G6E and C6orf15; see `projection-audit.tsv` for exact per-haplotype records. Native GenArk evidence remains archived for follow-up but was not merged into the figure.

An independent audit reconstructs all 9,196 projected exon coordinates directly from raw regional GFFs and aligns the re-extracted spliced DNA from the original source/target FASTAs. Every accepted model also has at least 95% global spliced-DNA identity (minimum 95.057%). This is sequence support, not experimental validation of a gene model. The original 8,894 transcript records and 65,643 exon records retain every original field. The existing 285 curated MHC heads, source statuses and shared head-row template are unchanged. All context and VR annotations remain below the MHC anchors, with a common bp scale. The prior shared-row export is preserved in `figures/before-alt-context/`.

These are MHC class I/II loci, not V(D)J genes. Six ape species have paired h1/h2 tracks; human remains CHM13 h1 only. Source annotation and projected-model counts have different ascertainment and must not be used as direct gene gain or loss estimates.

## Follow-up recovery of fourteen transfers

The final renderer reads `gene_models_with_recovered_context/`, which preserves every original field of the 9,850 prior transcript models and 74,839 exons and adds five supported alternative models with 15 exons. Recoveries are PABh2 LY6G6E/OR5V1/MUCL3, PPYh2 MUCL3 and GGOh2 MUCL3. These shorter released source isoforms satisfy the original 95% coverage/identity criteria plus an independent 95% global spliced-DNA identity and strand/synteny checks. They do not establish the original longest representative boundaries. There are now 961 strict context projections, nine unresolved complete models and 2,320 native context arrows.

Native Augustus predictions and RNA were checked for all 14 cases. No complete Augustus model was safe to add. Same-species IsoSeq evidence was realigned from bounded source-primary read extracts; some reads support partial loci, but complete target transcript boundaries and haplotype origin remain unresolved. Native RefSeq mRNA tracks provide cross-species alignment evidence, not ape-specific experimental RNA. Four mucin loci have repeat-expanded candidate exons with exact syntenic boundaries but below-threshold global identity; they remain explicitly separate candidates. Bonobo testis was not queried because of a coarse-index download budget; negative RNA evidence is not an absence claim.

See `context_recovery/README.md` and `gene_models_with_recovered_context/recovery-audit.tsv` for all 14 decisions, raw source links, RNA limitations, native candidate coordinates and regeneration instructions. `figures/before-unresolved-recovery/` preserves the previous export. Fresh sequence/coordinate/rendering checks retain 285 curated MHC heads, six shared rows, native anchors, a common bp scale and 624 ribbons without crossing leaders.

## Nucleotide ribbons

Adjacent alignments were regenerated with minimap2 asm20, CIGAR output and no secondary records from the exact native extracted sequences. The input sequence coordinates and strand are checked independently. Ribbons split at indels of at least 1000 bp; blocks below 500 bp or parent alignments below 70% identity are omitted for display. Small internal indels remain inside ribbon envelopes. Filtered or unaligned regions are not evidence of biological sequence absence. These real sequence alignments are not the unavailable original ED3c plot data. Each command, raw PAF and alignment log is retained.

## Reproduce

From the isolated worktree root. The original seven-track representative selection is archived in `figures/before-alt-context/representative-models.tsv` and is the fixed projection input. Recreating this input from raw source data requires the earlier representative-selection renderer; do not replace it with the current merged output.

The reviewed Liftoff environment is separate from the user's Python environment. To recreate it, create a virtual environment with a Python compatible with the recorded packages in `gene_models_with_projected_context/liftoff_runtime.json`, install that dependency set, and install Liftoff from the pinned source commit with `pip install --no-deps git+https://github.com/agshumate/Liftoff.git@94d6073ee2400c438d181a67d3af1e362197d839`. Use minimap2 2.31-r1302. Supply the resulting executable via `--liftoff`; the local reviewed runtime path below is temporary and can be recreated. Commands reject stale projection caches when input FASTAs, windows, GFFs or tool provenance differ.

Run:

```sh
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/fetch_mhc_continuous.py
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/fetch_mhc_continuous_genes.py --windows data-raw/ape_paper/results/mhc_continuous/windows.tsv --out data-raw/ape_paper/results/mhc_continuous/gene_models --ape-source catRefGene
# Liftoff source commit 94d6073ee2400c438d181a67d3af1e362197d839, minimap2 2.31-r1302.
# Install in an isolated runtime; the reviewed run uses this temporary environment.
/tmp/ggexon-mhc-liftoff-venv/bin/python data-raw/ape_paper/project_mhc_context.py --liftoff /tmp/ggexon-mhc-liftoff-venv/bin/liftoff
/tmp/ggexon-mhc-liftoff-venv/bin/python data-raw/ape_paper/verify_mhc_context_projection.py
# Uses archived bounded native-prediction, RNA and alternative-isoform searches.
/tmp/ggexon-mhc-liftoff-venv/bin/python data-raw/ape_paper/recover_mhc_context.py
Rscript data-raw/ape_paper/render_mhc_continuous.R
/tmp/ggexon-mhc-liftoff-venv/bin/python data-raw/ape_paper/verify_mhc_context_recovery.py
python data-raw/ape_paper/render_mhc_tracing_details.py
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/test_mhc_continuous_utils.py
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/verify_mhc_continuous.py
```

The bounded human browser chain extracts and discovery metadata live in `source_metadata/mhc_continuous/`. No complete genome is downloaded. Original numerical source records, raw regional BEDs and checksums remain available for audit.
