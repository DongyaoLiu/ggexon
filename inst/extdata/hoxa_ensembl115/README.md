# HOXA Ensembl 115 Demo Dataset

This directory contains a small plot-ready dataset for the ggexon flagship
HOXA/Hoxa synteny demo.

Source: Ensembl release 115 GTF files from
<https://ftp.ensembl.org/pub/release-115/gtf/>.

Files:

- `hoxa_genes.tsv`: HOXA/Hoxa gene intervals for human, rhesus macaque,
  mouse, chicken, and green anole. The table includes original Ensembl genomic
  coordinates, plot-oriented coordinates, and `reference_gene` homology keys.
- `hoxa_links.tsv`: adjacent-species orthology/synteny links between matching
  HOXA groups.
- `hoxa_species.tsv`: species metadata, source URLs, assemblies, GTF scope, and
  source seqnames.
- `hoxa_homology.tsv`: one combined table of non-human Ensembl gene IDs mapped
  to human HOXA reference groups, suitable for `HomologyAnnotation()`.
- `annotations/*.gff3`: tiny original-coordinate GFF3 files, one per species,
  for building a real `SynSpecies` object without downloading full Ensembl GTFs.

The plotting coordinates in `xmin` and `xmax` are oriented so the clusters are
displayed in a comparable HOXA13-to-HOXA1 order. Original Ensembl coordinates
are retained as `genomic_start`, `genomic_end`, `genomic_strand`, and
`source_seqname`.

Green anole uses the full Ensembl GTF because its HOXA genes are annotated on
scaffold `GL343275.1` and are absent from the chromosome-only GTF.
