# HOX cluster expansion tutorial data

This directory contains the compact data for the replacement HOX-cluster
expansion tutorial. It compares human, mouse, chicken, spotted gar, zebrafish,
and amphioxus without nucleotide or synteny ribbons.

The inputs are pinned to Ensembl release 116 for the vertebrates and Ensembl
Metazoa release 63, BraLan2, for *Branchiostoma lanceolatum*. Rebuild and
curation details are in
`data-raw/hox_cluster_expansion/build-hox-cluster-demo.R` and its README.

## Files

- `hox_genes.tsv` is the primary plot table. It has 207 protein-coding gene
  rows. Direct plotting fields include `species_row`, `cluster_column`,
  `cluster_name`, `slot`, `x`, `genomic_x_start`, `genomic_x_middle`,
  `genomic_x_end`, and `strand`. It also retains gene, transcript, protein,
  assembly, selection, anchor, mapping, and source provenance.
- `hox_cds.tsv` contains 424 CDS pieces from the selected transcripts, ranked
  in transcription order.
- `hox_clusters.tsv` contains all 28 cells in the seven-row, four-column
  matrix. `cell_status` distinguishes `retained`, `cluster_not_retained`, and
  `structural_blank`. `annotation_gap_hox_slots` is separate from other empty
  slots.
- `hox_annotation_gaps.tsv` records 17 literature-expected slots without a
  safe model in the pinned annotation: four chicken, 12 spotted-gar, and the
  BraLan2 Hox13 slot. They are not interpreted as gene loss.
- `hox_expected_complement.tsv` records the functional source inventories used
  for completeness QA: human 39, mouse 39, chicken 39, spotted gar 43,
  zebrafish 49, and amphioxus 15.
- `hox_slot_states.tsv` records every Hox15-Hox1 slot in retained panels and
  distinguishes plotted models, source-annotation gaps, biological
  non-retention, gar HoxA14 lineage absence, and the unplotted gar HoxD14
  pseudogene.
- `manual_hox_mapping.tsv` documents eight unnamed Ensembl models rescued by
  stable-ID/xref evidence and physical cluster collinearity.
- `curated_transcript_exclusions.tsv` records the three unsafe merged gar
  transcripts excluded before selecting a coherent transcript.
- `hox_xref_conflicts.tsv` records the UniProt HoxA3/HoxA4 label conflict and
  the evidence supporting the retained HoxA3 assignment.
- `amphioxus_hox_mapping.tsv` lists every Hox1-Hox15 slot. Fourteen have
  BraLan2 gene models; Hox13 has `annotation_gap_no_gene_model` status.
- `hox_species.tsv` records exact releases, assemblies, source files, URLs,
  retrieval date, and SHA-256 checksums.
- `annotations/*.gff3` are compact GFF3 files for Syn-backed demonstrations.
  Each mRNA includes its Hox `slot`, cluster, mapping method, and anchor
  provenance.

## Matrix rows and columns

The rows are `human`, `mouse`, `chicken`, `gar`, `zebrafish_a`,
`zebrafish_b`, and `amphioxus`. The four equal-width display columns are `A`,
`B`, `C`, and `D`.

Zebrafish row `a` contains HoxAa, HoxBa, HoxCa, and HoxDa. Row `b` contains
HoxAb, HoxBb, HoxCb, and an explicitly non-retained HoxDb cell. This is the
published 49-gene, seven-cluster complement. The unnamed Ensembl-116 model
ENSDARG00000100358 is manually rescued as HoxAa4.

The chicken and spotted-gar annotations are intentionally not padded with
invented boxes. Published functional members that lack a safe source gene
model remain annotation-gap slots. In gar, ENSLOCG00000011824 is recorded as
an unsafe chimeric candidate spanning several expected HoxA positions and is
not assigned to any one slot.

The longest source transcripts for gar HoxA3 and HoxB3 are three-homeodomain
Hox4/Hox3/Hox2 merges. They are excluded and shorter coherent Hox3 isoforms
are selected; the Hox2/Hox4 slots remain gaps. The only gar hoxc6a transcript
merges HoxC9-like and HoxC6-like proteins, so it is excluded and both slots
remain gaps. The HoxA3 row also flags the conflicting UniProt A4a label.

The biological cluster identity for amphioxus remains `ancestral`, but its
single cluster is displayed in matrix column `A` as a compact layout anchor.
That placement does not assign it specifically to HOXA: it represents the
pre-duplication counterpart of the A--D clusters. Amphioxus columns B--D are
structural blanks. Its Hox13 slot is an annotation gap between mapped Hox12
and Hox14 models; it is not a biological-absence claim. Posterior amphioxus
numbering is a positional comparison and does not assert strict one-to-one
vertebrate orthology. All 14 plotted BraLan2 models retain their minus-strand
orientation, while `slot` independently aligns their Hox-number positions.

## Anchor provenance

The selected transcript is the greatest-genomic-span coding transcript with a
usable CDS and strand after the three documented unsafe gar merged transcripts
are excluded. Complete explicit GTF codons are preferred. Missing
codon records use terminal-CDS plotting fallbacks, with the source recorded in
`initiation_anchor_source`, `stop_anchor_source`, and the corresponding
fallback flags. The fallback source token is `terminal_CDS_positional_proxy`.
The default `x` is the genomic midpoint between those two anchors.
