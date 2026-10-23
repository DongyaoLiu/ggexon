# Fourteen unresolved MHC context transfers: recovery audit

Five gene contexts are now represented by supported **shorter released alternative isoforms**, transferred from the same species. They are inferred alternate-haplotype models, not validation of the failed original representative boundaries. All require ≥95% exon coverage and identity, independently ≥95% gap-inclusive global spliced-DNA identity, correct strand and the collinear locus. The existing 956 projections, all released annotation fields, 285 curated MHC heads, six shared rows, native axes and 624 ribbons are unchanged. The latest figure contains 961 projected context models and 2,320 grey context arrows in total.

## Decisions for all fourteen

RNA below refers to same-species experimental IsoSeq reads selected in source-primary intervals and realigned to the alternate MHC. It is **not** native target BAM coverage or haplotype-origin-resolved expression. Reported RNA counts denote partial overlap, not complete transcripts; fragments/read-through can overlap genes.

| Haplotype | Gene | Decision | Global identity of accepted alternative | Partial RNA reads | Reads with matching target splice junction | Native Augustus |
|---|---|---|---:|---:|---:|---|
| SSYh2 | MUC21 | Complete model unresolved | — | 1 | 1 | reject opposite strand prediction |
| PABh2 | LY6G6E | Alternative recovered | 100.000% | 0 | 0 | no native prediction at syntenic locus |
| PABh2 | OR12D3 | Complete model unresolved | — | 0 | 0 | no native prediction at syntenic locus |
| PABh2 | OR5V1 | Alternative recovered | 99.617% | 17 | 0 | no native prediction at syntenic locus |
| PABh2 | MUC21 | Complete model unresolved | — | 0 | 0 | no native prediction at syntenic locus |
| PABh2 | MUCL3 | Alternative recovered | 99.473% | 1 | 0 | no native prediction at syntenic locus |
| PABh2 | OR12D2 | Complete model unresolved | — | 0 | 0 | no native prediction at syntenic locus |
| PPYh2 | MUC21 | Complete model unresolved | — | 0 | 0 | no native prediction at syntenic locus |
| PPYh2 | MUCL3 | Alternative recovered | 99.511% | 1 | 0 | no native prediction at syntenic locus |
| GGOh2 | MUC21 | Complete model unresolved | — | 0 | 0 | reject opposite strand prediction |
| GGOh2 | MUC22 | Complete model unresolved | — | 0 | 0 | reject opposite strand prediction |
| GGOh2 | MUCL3 | Alternative recovered | 98.833% | 1 | 1 | partial exon corroboration only |
| PPAh1 | C6orf15 | Complete model unresolved | — | 0 | 0 | no gene specific native model |
| PPAh1 | OR2J2 | Complete model unresolved | — | 0 | 0 | no gene specific native model |

## What the evidence establishes

- **Five shorter alternatives:** PABh2 LY6G6E and OR5V1, plus MUCL3 in PABh2, PPYh2 and GGOh2. Independent gap-inclusive identity is 98.83–100%. Fifteen transferred exons are added. OR5V1 is a one-exon alternative; its overlapping RNA fragments cover only a small part and have no matching junction, so RNA does not establish its full model. The three MUCL3 alternatives contain four exons, LY6G6E two exons and OR5V1 one exon. The failed longer representative models are not reconstructed.
- **No complete Augustus replacement:** orangutan tracks have no predictions at the unresolved loci; siamang MUC21 and gorilla MUC21/MUC22 predictions use the opposite strand; the gorilla MUCL3 prediction fuses neighboring genes. Two exact predicted MUCL3 exons corroborate its locus only. Native predictions are therefore not added wholesale.
- **Four structurally plausible but subthreshold mucins:** PABh2, PPYh2 and GGOh2 MUC21, and GGOh2 MUC22 have exactly mapped exon endpoints in collinear MAPQ60 genome alignments and canonical introns. Human RefSeq alignments support the same loci. Retaining within-exon insertions gives global identities 90.269%,94.574%,83.674%,72.282%, respectively. MUC21 middle-exon length changes are +315,+135,+630bp (7,3,14units of45 bp); this is consistent with repeat expansion, not proof of a missing gene. These candidate models are kept in the audit rather than promoted past the 95% criterion. See `source_metadata/mhc_unresolved_recovery/native_predictions/mucin_boundary_report.md`.
- **Siamang MUC21:** one qualifying realigned RNA read matches one target-model junction and approximately 62.9% of the diagnostic candidate's exonic bases. This supports a transcribed locus but not complete transcript boundaries; the original transfer covers only ~77% and remains unresolved.
- **Other unresolved loci:** PABh2 OR12D2/OR12D3 and PPAh1 C6orf15/OR2J2 lack sufficient evidence for a complete accepted model. Alternative OR12D2/OR2J2 mappings have low identity and possible paralog ambiguity. These are not gene absence calls.

## RNA scope and limitations

All14exposed IsoSeq BAM headers lack the alternate MHC chromosome. Thirteen BAM tracks were queried with small indexed byte ranges on source-primary chromosomes. Bonobo testis was not queried because its coarse index implied a~200 MB range fetch; the other two bonobo tracks returned no candidate reads. The retained RNA evidence is ~1.4 MB; no whole BAM or genome was downloaded. Negative results are tissue-, sampling- and source-mapping-dependent. This approach can miss alternate-specific reads that failed mapping to the primary.

Source FASTQs contain original forward read sequence. Re-alignment uses minimap2 splice:hq, -uf and --eqx against the exact alternate regional FASTA. Even partial support requires source-locus same-strand exonic assignment, a primary target alignment with MAPQ ≥20, ≥95% gap-inclusive sequence identity, ≥90% query aligned, target annotation-strand agreement and at least 100 bp exonic overlap. Intron-only and antisense overlaps are excluded. Partial RNA is not used to manufacture complete model boundaries. Species and transcriptomic provenance were verified through ENA; the reads need not originate from the sequenced haplotype or individual.

## Files and reproduction

- `gene_models_with_recovered_context/recovery-audit.tsv`: all 14 decisions, native-prediction assessment, exact cross-species transcript accessions and target RNA support.
- `recovered-models.tsv`, `candidate-assessments.tsv`: selected and rejected alternative candidates, native BED0 spans, source IDs, coverage, identities and raw GFF hashes.
- `rna-realignment-qc.tsv`: all overlapping target RNA alignments, including excluded ones, strand/quality/overlap and matching-junction counts.
- `recovery_manifest.json`, `verification.json`: input hashes, criteria, limitations and independent coordinate/DNA/layout checks.
- `context_recovery/alternative_isoforms/`:17 alternative searches, native source model metadata, regional GFFs, commands and logs.
- `context_recovery/diagnostics/`: below-threshold original Liftoff mappings retained for diagnosis.
- `context_recovery/rna_realignments/`: small pooled source FASTQs, target SAMs, commands and logs.
- `source_metadata/mhc_unresolved_recovery/`: exact Augustus queries, BAM-header audit, indexed source-region read extracts, read QC, ENA provenance, RefSeq metadata and regeneration notes.

From the worktree root, with the previously pinned Liftoff runtime and archived bounded inputs:

```sh
/tmp/ggexon-mhc-liftoff-venv/bin/python data-raw/ape_paper/recover_mhc_context.py
Rscript data-raw/ape_paper/render_mhc_continuous.R
/tmp/ggexon-mhc-liftoff-venv/bin/python data-raw/ape_paper/verify_mhc_context_recovery.py
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/verify_mhc_continuous.py
/Users/liudongyao/miniconda3/bin/python data-raw/ape_paper/render_mhc_tracing_details.py
```

The recovery script consumes archived searches and hashes; it does not silently re-fetch all evidence. Raw alignments can be regenerated with each archived `command.json` using the pinned runtime/inputs. Previous figures are retained in `figures/before-unresolved-recovery/`. The original 956-model projection data and their verifier remain intact.
