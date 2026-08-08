# cox1/COX1 Yeast Pairwise Demo Candidate

Generated from PomBase and SGD downloads whose URLs, retrieval dates,
and exact MD5 checksums are recorded in `cox1_provenance.tsv`.

This dataset supports a pairwise ggexon tutorial with two mitochondrial
annotation tracks and one middle `geom_nuclink()` panel. Unlike the CD44
human/mouse tutorial, this is not an alternative-transcript example. It
shows conserved mitochondrial cytochrome c oxidase subunit 1 coding sequence
across different organellar intron architectures in fission and budding
yeast.

Key files:

- `cox1_species.tsv`: species, source, gene, and plotted window metadata.
- `cox1_genes.tsv`: gene-level coordinates.
- `cox1_transcripts.tsv`: one protein-coding transcript per species.
- `cox1_cds_exons.tsv`: CDS intervals from PomBase and SGD.
- `cox1_introns.tsv`: annotated intron intervals from PomBase and SGD.
- `cox1_plot_exons.tsv`: plot-ready CDS intervals for `geom_exon()`.
- `cox1_nuclinks_lastz.tsv`: LASTZ-derived genomic interval links retained at alignment length >= 40 bp and identity >= 50%.
- `cox1_exon_homology_candidates.tsv`: exon-homology candidates from overlaps
  between LASTZ blocks and CDS intervals.
- `cox1_exon_homology_ranked.tsv`: one row per exon-pair candidate with
  reciprocal-best ranks.
- `annotations/*.gff3`: compact selected-transcript GFF3 files.
- `sequences/*.fa`: genomic DNA windows used for LASTZ.
- `cox1_provenance.tsv`: source URLs, checksums, dates, and portable tool provenance.
