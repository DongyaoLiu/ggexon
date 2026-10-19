AS3MT PLOTTING HANDOFF
Prepared 2026-10-18. Main machine-readable file: as3mt_plot_spec.json.

RECOMMENDED FIGURE
Title: Human AS3MTd2d3: exon skipping and neural phenotypes.
A: Compare the selected chimpanzee full transcript, human full transcript,
and paper-defined human AS3MTd2d3 model. Emphasize E1-to-E4 skipping of
E2 and E3. Show CDS as thick boxes, UTR as thin boxes, and preserve the
full-reference exon numbering. Zoom E1-E4 or mark intron compression.
B: Align the proteins to full-reference residues 1-375; the assigned d2d3
protein corresponds to residues 103-375, losing 102 N-terminal amino acids.
C: Plot the deposited human-organoid SOX2 and TUJ1 fractions, using
individual reported values plus mean and SEM. Keep the sampling-unit note.
The JSON also provides optional spindle-angle data and rat-neuron spine
means. Choose panels according to the intended figure; all are not required.

FILES
as3mt_plot_spec.json: identifiers, exon and CDS coordinates, source data,
provenance, measured phenotype values, figure references and plotting rules.
human_AS3MT_GRCh38.bed: BED12 for human full and reconstructed d2d3 models.
chimp_AS3MT_Pan_tro_3.0.bed: BED12 for the selected chimp reference.
AS3MT_reference_and_derived_sequences.fasta: human reference sequences and
explicitly reconstructed d2d3 sequences. See JSON for derivation assumptions.
sources/: raw reference annotation/sequence snapshots and selected original
supplementary spreadsheet cells supporting the quantitative panels.

COORDINATES AND ANNOTATION STATUS
JSON genomic coordinates are 1-based inclusive. BED uses 0-based half-open
coordinates. Its thick interval includes the terminal stop codon.
Human: GRCh38, chr10,+; NM_020682.4 / ENST00000369880.8.
Chimp: Pan_tro_3.0, chr10,+; ENSPTRT00000007262.6.
Use separate genomic axes for the two assemblies, or clearly mark a schematic
alignment by corresponding exon number. Human coordinates cannot be reused
as chimp coordinates. Raw models were retrieved from Ensembl release 116.

AS3MTd2d3 is a paper-defined isoform, reconstructed here by retaining current
canonical E1,E4-E11 and the selected reference transcript ends. No matching
stable accession was independently identified among the current models
examined. The reconstructed 2,281 nt length is conditional on these chosen
ends and reference repeat allele; it is not a universal measured isoform size.

E2 and E3 contain 41 + 128 = 169 nt. Their skipping removes the original initiation
codon, whose A is in E1 and TG in E2. The assigned alternative start is
chr10:102872584-102872586 in E4; it produces 273 aa. Do not treat the event
as a 306 nt in-frame exon deletion. Shared stop: chr10:102900698-102900700.

EVIDENCE LABELS
Li 2016 Supplementary Figure 4 compares human DLPFC with chimp pooled tissues
including brain. Label the chimp result 'E1-E4 not detected in examined
RNA-seq'. Annotation absence alone does not establish biological absence.
Human case-control expression is an association. The organoid and rat-neuron
experiments test increased isoform expression and are not interspecies
phenotype measurements. Healthy humans also express the short isoform.

HUMAN ORGANOID DATA
Kim 2025 Table S5, sheets Figure S8B / Figure S8C, supplies original values for SOX2+
or TUJ1+ cells among GFP+ cells. Control has 7 reported values; overexpression
has 11. The paper does not explicitly identify the sampling unit, so do not
call these 7/11 independent donors or organoids. SEM is calculated from the
deposited values; the original legend explicitly specifies SEM.
The optional Figure 5F data are percentages in three cleavage-plane-angle
bins. Fractions within each sample sum to 100%; retain that dependency.

RAT-NEURON OPTION
Cai 2021 Figure 3 provides exact published means. The JSON deliberately gives
no per-neuron values or error bars because these were not recovered for this
handoff. Use mean-only marks with n = 20/19 neurons and the reported tests.

ORIGINAL PAPERS AND FIGURE DATA
Li M et al. Nature Medicine 2016;22:649-656. doi:10.1038/nm.4096.
Fig. 2a: isoform architecture. Fig. 3d: diagnosis-associated expression.
Supplementary Fig. 3a: VNTR. Supplementary Fig. 4: species comparison.
Supplementary Table 12: enzyme activity. Original source-data links are in JSON.
Cai X et al. Schizophrenia Bulletin 2021;47:219-227.
doi:10.1093/schbul/sbaa098. Fig. 3: rat hippocampal dendritic spines.
Kim S et al. Science Advances 2025;11:eadp8271.
doi:10.1126/sciadv.adp8271. Fig. 5F and S8B-C: human organoid phenotypes.

All numerical phenotype observations here come from published results or
deposited source tables. This package does not contain measured human-versus-
chimpanzee expression counts or evidence of species-specific disease rates.
