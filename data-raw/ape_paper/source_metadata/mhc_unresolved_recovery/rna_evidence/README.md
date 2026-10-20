# RNA evidence audit for the 14 unresolved MHC context transfers

Date: 2026-10-20. Research/QC only. No model, curated MHC head, row, anchor, scale, or figure files were edited.

`unresolved14_rna_candidates.tsv` has exactly one row for each unresolved source-model transfer. It records the exact RefSeq accession, source species/symbol, native target chromosome, BED0 span, strand, raw blockSizes/blockStarts, absolute alignment blocks, source URLs, diagnostic-Liftoff locus agreement, and RNA evidence assessment. `symbol_matched_xenoRefGene_candidates.raw.bed` retains the ten exact original target-coordinate records.

## Interpretation

- Nine matches are human RefSeq transcripts (MUC21, MUC22, MUCL3, OR5V1); one LY6G6E match is cattle. Every hit overlaps the corresponding low-threshold Liftoff locus on the same strand. These support locus candidacy, not direct ape annotation. The UCSC xenoRefGene track uses translated BLAT; its blocks are alignment blocks and must not automatically become native exon boundaries. No symbol-matched record was found for PABh2 OR12D3/OR12D2 or PPAh1 C6orf15/OR2J2. An olfactory receptor paralog must not be substituted.
- All 14 public IsoSeq BAM headers were checked. They contain the source primary-haplotype GenBank chromosome and **do not contain the unresolved target-haplotype chromosome**. The chromosome lists and original minimap2 command lines are in `native_rna_bam_header_audit.json` and individual `*.header.sam` files. A diploid browser track location is not proof that the underlying BAM was aligned to both haplotypes.
- Species-specific experimental IsoSeq reads were therefore extracted from the original source-model intervals, using small BAM-index byte ranges. These are same-species RNA, with unresolved haplotype origin; they are **not target-haplotype alignments**. Target realignment and assignment are required before use.
- Thirteen BAM tracks were queried to completion for these source intervals. Bonobo testis was excluded by the explicit budget: its coarse index required 1,135 merged requests / approximately 152 MB of planned BAM ranges. Bonobo IsoSeq 1 and 2 were queried. The completed extraction pass fetched 24.6 MB of BAM byte ranges, plus indexes. Index sizes/checksums are saved; the 57.8 MB of downloaded indexes were removed after QC so this evidence directory stays small. No full BAM was downloaded.
- Source RNA candidates require a primary alignment, MAPQ >= 20, agreement with the source model strand, and >=100 aligned bp overlapping original source exons. This is a candidate filter, not a gene-identity or full-transcript assertion. The read-level table also gives fraction of source exonic bases covered and exact source-junction concordance (both junction endpoints within 5 bp). `source_envelope_spanned` describes genomic span only.
- No absence call follows from zero reads: the sampled tissues, primary alignment, multi-mapping and gene expression all limit this screen. Opposite-strand reads and reads spanning a locus entirely through an intron were retained in permissive evidence files but excluded from candidate FASTQs.

## Candidates for target realignment

| Target | Gene | Candidate reads | Best source exon fraction | Source-junction evidence | Assessment |
|---|---|---:|---:|---|---|
| SSYh2 | MUC21 | 1 | 41.9% | 2 matched junctions | Partial transcript evidence; strongest MUC21 lead |
| PABh2 | OR5V1 | 17 | 13.0% | 0 matched junctions | Weak fragments only; gene identity unconfirmed |
| PABh2 | MUCL3 | 1 | 26.6% | 0 matched junctions | Partial candidate; repeat/paralogy checks needed |
| PPYh2 | MUCL3 | 3 | 67.3% | 1 matched junction in best read | Partial transcript evidence |
| GGOh2 | MUCL3 | 1 | 44.4% | 1 matched junction | Partial transcript evidence |

All other unresolved loci lack qualifying reads in the sampled source tracks. Gorilla MUC22 has permissive interval-overlapping RNA records, but they fail source-exon/strand QC; they are not established MUC22 transcripts. No mucin candidate spans and validates the complete original source transcript structure.

Use `TRACK.GENE.source_sense_exonic.fastq` for target realignment. These FASTQs restore original read orientation with pysam `get_forward_sequence()` and `get_forward_qualities()`. The per-BAM `*.source_loci.fastq` files are intentionally more permissive and include primary MAPQ20 interval-overlapping reads that later fail exon/strand QC. Consult `source_rna_read_qc.tsv` before assigning a transcript.

## Files and reproduction

- `extract_indexed_rna.py`: BAI-bin selection, bounded HTTP Range retrieval, BGZF/BAM decoding, regional SAM/FASTQ/read evidence. Uses the original source representative models. Enforces a 10 MB or 100-range maximum per BAM.
- `build_candidate_report.py`: raw RefSeq joins, target/diagnostic-locus checks, source exon/strand/junction QC and fourteen-row report.
- `primary_rna_query_intervals.json`: exact source model ID, source GenBank chromosome, BED0 interval and strand for all 14.
- `primary_rna_extraction_manifest.json`: BAM URLs, exact byte/virtual-offset ranges, query outcomes and explicit skipped track.
- `source_rna_read_qc.{json,tsv}`: every permissive read/gene overlap, flags, MAPQ, CIGAR, absolute blocks/junctions, source exon overlap and QC.
- `*.source_loci.sam`, `*.source_loci.fastq`, `*.source_loci.evidence.json`: small regional read extracts and metadata.

Run from any working directory with the Python runtime containing pysam:

```sh
/Users/liudongyao/miniconda3/bin/python /Users/liudongyao/.codex/worktrees/ape-integrated-annotations/ggexon/data-raw/ape_paper/source_metadata/mhc_unresolved_recovery/rna_evidence/extract_indexed_rna.py
/Users/liudongyao/miniconda3/bin/python /Users/liudongyao/.codex/worktrees/ape-integrated-annotations/ggexon/data-raw/ape_paper/source_metadata/mhc_unresolved_recovery/rna_evidence/build_candidate_report.py
```

Reproduction downloads BAM indexes again (57.8 MB total); full BAMs remain remote. The saved report does not incorporate later target RNA realignment performed by other recovery steps.

## Primary sources

- [T2T Browser native RNA track definitions](https://github.com/marbl/T2T-Browser/tree/main/T2Tgenomes) and [IsoSeq methods](https://github.com/marbl/T2T-Browser/blob/main/T2Tgenomes/html/isoseq.html). Definitions and method HTML are saved locally. These document minimap2 alignment of consortium PacBio IsoSeq data.
- [Canzar lab transcriptome analysis](https://github.com/canzarlab/apes_transcriptome_analysis): independent StringTie assemblies of experimental IsoSeq reads. The sampled GTF header uses RefSeq primary chromosome names; the repository exposes Git LFS GTF files of roughly 100 MB. These were not downloaded or treated as target-haplotype annotation. README saved as `canzarlab_README.md`.
- [Study data availability](https://github.com/marbl/Primates): transcriptome BioProjects PRJNA902025 and PRJNA1016395. Raw archive availability does not itself establish a gene model at an unresolved locus.
- RefSeq accession and native GenArk bigBed source URLs are given individually in the fourteen-row report; the saved `mhc_context_search/ncbi/*.genark.xenoRefGene.html` files document translated-BLAT construction and multi-placement filtering.

### Experimental RNA sample provenance

ENA run metadata were fetched for every run supplying a retained sense/exonic candidate. All four are annotated RNA-Seq / TRANSCRIPTOMIC, confirming that these are species-specific experimental RNA and not human transcript projections. The exact TSV responses and API URLs are saved in `*.ENA.metadata.tsv` and `ENA_sample_metadata_manifest.json`.

| Run | Organism in ENA | Sample | BioProject |
|---|---|---|---|
| [SRR27178656](https://www.ebi.ac.uk/ena/browser/view/SRR27178656) | Symphalangus syndactylus | Jambi cell line | PRJNA1016395 |
| [SRR22452368](https://www.ebi.ac.uk/ena/browser/view/SRR22452368) | Pongo abelii | Testis; sample title retains historical name Pongo pygmaeus abelii | PRJNA902025 |
| [SRR22452369](https://www.ebi.ac.uk/ena/browser/view/SRR22452369) | Pongo pygmaeus pygmaeus | Bornean orangutan testis | PRJNA902025 |
| [SRR22452370](https://www.ebi.ac.uk/ena/browser/view/SRR22452370) | Gorilla gorilla gorilla | ENA title: Model organism or animal sample from Gorilla gorilla gorilla; browser track labels testis | PRJNA902025 |

Taxon verification establishes species origin, not chromosome/haplotype origin or a complete transcript structure. Reads in noncandidate tracks were not individually run-metadata audited.
