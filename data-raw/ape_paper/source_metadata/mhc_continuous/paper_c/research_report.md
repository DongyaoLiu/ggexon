# Extended Data Figure 3c source audit

## Result

The three discordant regions are available as exact machine-readable numbers in **Supplementary Table XI.59**, worksheet `59` of `41586_2025_8816_MOESM4_ESM.xlsx`. They are not figure-estimated intervals. The table explicitly labels the chromosome column **Chromosome (T2T-CHM13)**. Neither its header nor its rows explicitly states whether Start is zero-based or one-based. Every reported Size equals End minus Start. Preserve these literal numbers and the convention uncertainty; do not silently shift them or substitute HG002 coordinates.

| Region | Chromosome | Start (literal source) | End (literal source) | Reported size |
| --- | --- | ---: | ---: | ---: |
| VR1 | chr6 | 28669878 | 29944878 | 1275000 |
| VR2 | chr6 | 30924878 | 31319878 | 395000 |
| VR3 | chr6 | 32274878 | 32934878 | 660000 |

The same worksheet gives narrower coalescent subregions I:29295878–29302378; II:30925378–31026878; III:32310378–32325378; IV:32916878–32937878. These are separate intervals and are not replacements for VR1–3.

The **exact original full reference extraction boundaries of panel c were not found in the inspected public source material**. The caption's 5.31 Mbp should not be converted to a precise genomic interval. The available caption does not say whether this is an ungapped human reference extraction span, gapped alignment length, or rounded plotting extent. No original MHC-specific FASTA/PAF/MAF/MSA files or panel-c plotting script were found in the inspected repositories. This is a negative result limited to the sources listed below, not a claim that the data do not exist elsewhere.

## Why the 5.31 Mbp panel cannot be identified with the manual annotation region

Supplementary information PDF printed p85 (PDF page86) defines the MHC genomic region for manual annotation between GABBR1 and KIFC1. Its subsequent printed coordinates are `2937649-3331258`, attributed to human T2T-CHM13v2.0 chr6. These literal coordinates are inconsistent with the named MHC genes. The same printed numbers occur in the official browser track description, so this is an upstream text issue, not PDF extraction dropping a digit. Do not repair them by guessing missing digits.

The separate phylogenetic-reconstruction methods (printed pp94–95; PDF pages95–96) describe a **5.5 Mbp** human T2T sequence, 500-bp nonoverlapping bins, pairwise minimap2 extraction of ape homologs, local MAFFT optimization, and concatenation into 5-kbp windows. These methods include HG002 and say siamang was omitted for this particular inference. They do not provide exact whole-locus endpoints or declare 5.31 Mbp to be MSA length.

The primary ED3 raster (`ed3.jpg`) visibly contains **15 alignment rows**: CHM13, two HG002 haplotypes and 12 ape haplotypes, including siamang. Its annotation panel labels **ZNF311** near the left end and **GRM4** near the right end. Its axis is relative 0 to approximately5.3 Mbp; it provides no chromosome-origin number. Those broader flanking landmarks establish that GABBR1–KIFC1 alone is not an adequate reconstruction of the displayed extent. They are landmarks, not exact cut coordinates.

VR1 begins upstream of GABBR1 in the available CHM13 gene annotation. That is consistent with an analysis that includes broader flanks, and is not evidence to relabel the table as HG002. The source's conflicting extents and malformed coordinates prevent a stronger conclusion about the origin.

## Defensible reconstruction

1. Use exact published VR1–3 values, showing the source table assembly label and the indexing caveat in provenance.
2. Choose and explicitly state a reproducible CHM13 reference window broad enough to include ZNF311, GRM4 and all three VRs, using actual reference gene bounds and a documented flank rule. Label this a reconstruction, not the exact ED3c extraction. Do not choose endpoints merely to force5.31 Mbp or derive an origin from repeated decimal endings.
3. Lift that reference window to each of the12 ape haplotypes using public nucleotide chain alignments; keep each native genomic span and orientation. Display13 rows if requested, omitting HG002 as an intentional difference from the original15-row panel.
4. Use public browser chain blocks or regenerated nucleotide alignments for ribbons, explicitly naming their source. These are defensible sequence-level ribbons, but should not be described as the original panel-c alignment.
5. Preserve native distances on a common bp scale, all available gene models and a separate exact VR interval track. No phylogenetic trees are necessary.

## Inspected primary sources

- Article and ED3 caption: https://www.nature.com/articles/s41586-025-08816-3 (Extended Data Fig3; methods narrative and data/code availability).
- Primary ED3 raster: https://media.springernature.com/full/springer-static/esm/art%3A10.1038%2Fs41586-025-08816-3/MediaObjects/41586_2025_8816_Fig11_ESM.jpg
- Supplementary information: https://media.springernature.com/original/springer-static/esm/art%3A10.1038%2Fs41586-025-08816-3/MediaObjects/41586_2025_8816_MOESM1_ESM.pdf
- Supplementary tables: https://media.springernature.com/original/springer-static/esm/art%3A10.1038%2Fs41586-025-08816-3/MediaObjects/41586_2025_8816_MOESM4_ESM.xlsx (worksheet59).
- https://github.com/marbl/Primates recursive tree and README. No MHC-specific analysis directory or data found; links to general alignments and browser resources.
- https://github.com/marbl/T2T-Browser recursive tree, `src/MHC`, and `T2Tgenomes/html/mhcGene.html`. Manual gene interval tables and colors are public; the full original panel-c region alignment is not supplied there.
- https://github.com/T2T-apes/ape_pangenome recursive tree and alignment/conservation READMEs. Public whole-genome wfmash mappings/alignments/chains/MAFs are linked; no MHC-specific original panel-c extraction found.
- https://github.com/makovalab-psu/T2T_primate_autosomes recursive tree. Domain-specific non-B DNA, NUMT and divergence analyses; no MHC panel-c source found.
- General Cactus HAL/MAF/chain dataset linked by the article: https://cglgenomics.ucsc.edu/february-2024-t2t-apes
- General public wfmash alignment repository: https://garrisonlab.s3.amazonaws.com/index.html?prefix=t2t-primates/wfmash-v0.13.0/alignments_fixedSiamang/
- General public wfmash chains: https://garrisonlab.s3.amazonaws.com/index.html?prefix=t2t-primates/wfmash-v0.13.0/chains_fixedSiamang/

## Local evidence

`Supplementary_Table_XI_59_exact.tsv` is an exact worksheet export. `table59_provenance.json` identifies the original workbook and its SHA256. The PDF, extracted text, figure raster, browser track description, relevant READMEs, and recursive repository trees are preserved in this directory. No renderer or IG/TR files were changed for this research task.
