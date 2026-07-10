# CD44/Cd44 Pairwise Isoform Demo Candidate

Generated from Ensembl REST release 116 and UCSC sequence API.
Genomic DNA windows include a strand-aware 20 kb promoter-side flank and 10 kb 3-prime-side flank around each gene model.

This dataset supports a pairwise ggexon tutorial with two annotation tracks
and one middle `geom_nuclink()` panel. The selected isoforms are chosen by
a reproducible rule: keep the Ensembl canonical protein-coding transcript,
add the fewest-exon and most-exon full-locus protein-coding transcripts,
then add the remaining transcript that maximizes exon-set Jaccard distance.

Key files:

- `cd44_selected_isoforms.tsv`: representative isoforms selected for plotting.
- `cd44_selected_exons.tsv`: plot-ready exon intervals for those isoforms.
- `cd44_selected_unique_exons.tsv`: unique exon intervals in the selected isoforms.
- `cd44_common_exons.tsv`: exons present in all selected isoforms per species.
- `cd44_nuclinks_lastz.tsv`: LASTZ-derived genomic interval links retained at alignment length >= 80 bp and identity >= 50%, with identity bins and human gene-region labels.
- `cd44_exon_homology_candidates.tsv`: exon-homology candidates from overlaps
  between LASTZ blocks and Ensembl exons.
- `cd44_exon_homology_ranked.tsv`: one row per exon-pair candidate with
  reciprocal-best ranks and common/variable exon flags.
- `annotations/*.gff3`: compact selected-transcript GFF3 files.
- `sequences/*.fa`: genomic DNA windows used for LASTZ.
- `cd44_provenance.tsv`: source URLs and local command provenance.
