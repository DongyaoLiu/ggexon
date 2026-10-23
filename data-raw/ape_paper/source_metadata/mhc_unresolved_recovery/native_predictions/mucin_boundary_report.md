# Mucin exon boundary and length diagnostic

These four source-syntenic native candidates are diagnostic evidence only. They are not strict 95% identity recoveries, production annotation edits, or target-haplotype RNA observations.

All 26 source exon endpoints map exactly through one collinear MAPQ60 PAF alignment per gene. No nearest-base rescue, endpoint trimming or exon truncation was needed. Genomic target insertions inside each source exon were retained inside the corresponding target exon, rather than split into apparent introns. Every mapped intron has target GT/AG boundary dinucleotides. The mapped gene spans and splice boundaries are thus sensible as same-species syntenic candidates, while precise transcript use still requires native expression evidence.

Spliced DNA was extracted from the actual regional source and target FASTA files and globally aligned with parasail Needleman-Wunsch (match2, mismatch-3, gap-open5, gap-extension1). Identity below is exact matches divided by the entire global alignment length, including all insertion/deletion columns. All lengths are nucleotides; intervals are native BED0.

| Track/gene | Native span | Source / target spliced length | Net target change | Global identity |
| --- | --- | --- | --- | --- |
| PABh2 MUC21 | CM054661.2:30960004-30966052 (+) | 3239 / 3566 | +327 | 90.269% |
| PPYh2 MUC21 | CM054612.2:31035739-31041701 (+) | 3341 / 3476 | +135 | 94.574% |
| GGOh2 MUC21 | CM054563.2:41093511-41100141 (+) | 3474 / 4103 | +629 | 83.674% |
| GGOh2 MUC22 | CM054563.2:41121114-41147617 (+) | 5599 / 7735 | +2136 | 72.282% |

The principal length changes lie in the middle exon: +315bp in PAB MUC21, +135bp in PPY MUC21, +630bp in GGO MUC21 and +2136bp in GGO MUC22. The complete MUC21 target middle exons show strongest DNA periodicity at45bp, with82.4-84.2% matching bases at that shift; the net middle-exon changes correspond to7,3 and14 copies of45bp. GGO MUC22 shows strongest periodicity at30bp (73.0% matching bases), with +2136bp of internal target insertions. These observations are consistent with within-exon repeat-length changes. Small differences also occur elsewhere (PAB final exon +12bp; GGO MUC21 final exon -1bp). They are not evidence for pseudogene status or functional loss.

The source-base-only PAF identity statistic missed inserted target bases. Consequently, it was close to99% for several genes even though the full target transcript sequences are substantially longer. The gap-inclusive global identities are all below95%; PPY MUC21 is only slightly below, but remains below. The concrete failure mode is length divergence in homologous repetitive exons. This supports a biological explanation for stringent transfer failures without relaxing the acceptance criterion.

Human RefSeq sequence alignments independently support all four native loci:

- PAB MUC21 NM_001322371.2 overlaps99.66% of the aligned human exonic sequence. The candidate begins10bp later and ends629bp later than that human alignment. Both internal exon starts and both internal exon ends match human-alignment boundaries.
- PPY MUC21 NM_001322370.2 overlaps99.50% of the aligned human exonic sequence. The candidate begins10bp later and ends629bp later. The middle-exon end and final-exon start agree; the human middle-exon alignment begins4bp earlier than the source-mapped candidate.
- GGO MUC21 NM_001322371.2 overlaps99.69% of the aligned human exonic sequence. The candidate begins10bp later and ends627bp later. Both internal exon starts and both internal exon ends match human-alignment boundaries.
- GGO MUC22 NM_001198815.1 overlaps100% of the aligned human exonic sequence. The candidate begins194bp earlier and has the same native end. Every candidate exon end and all three downstream exon starts match human-alignment boundaries.

These are human transcript-to-ape genomic alignments, not ape RNA reads. Their extra blocks within repetitive exons must not automatically become introns. Terminal differences can reflect source transcript boundaries or cross-species alignment coverage and should not be interpreted as native transcript-end validation. Existing ape Iso-Seq BAMs use the primary haplotype; any remapping of their reads to the alternate haplotype must be labeled separately.

Exact exon arrays, per-exon source coverage, all boundary flags, global alignment counts, human RefSeq blocks and overlap measurements are in `mucin_exact_boundary_diagnostics.json` and `mucin_exact_boundary_exons.tsv`. Actual extracted source/target spliced sequences and complete global alignments are saved alongside them. `diagnose_mucin_boundaries.py` regenerates the evidence without writing annotation or plot files.
