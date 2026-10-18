# Dense HLA-A multiallelic lollipops

Offline data and helpers for `vignettes/hla-a-lollipop-demo.Rmd`.

## Eligibility and counting

The pinned IPD-IMGT/HLA 3.66.0 release contains 5,998 eligible named alleles:
365-residue precursors, twenty standard amino acids only, names ending in a
numeric field. Names with letter suffixes and partial, ambiguous, or other-length
sequences are excluded. Every eligible sequence is verified against the official
alignment to share the reference's gap pattern. Counts include every named allele
once, even when multiple alleles encode the same mature protein. There are 3,188
distinct mature proteins, 1,212 substitutions, and 334 variable residues.

Comparison reference: A*02:01:01:01. Coordinates: mature residues 1–341, obtained
by removing the 24-residue signal peptide. Counts describe database records,
not individuals or population frequencies. The additional gene-model view uses
allele-local coordinates from the exact HLA00005 reference, not GRCh38.

## Files

- `hla_a_selected_proteins.fasta.gz`: gzip-compressed original FASTA records,
  unchanged after decompression, from commit
  `d972ae287948dc2e5468a40ff3b3d27227541189`.
- `hla_a_alleles.tsv`: exact names, groups, reference flag, lengths, and per-sequence
  SHA-256 values. Identical proteins are deliberately not deduplicated.
- `hla_a_domains.tsv`: UniProt P04439 structural boundaries shifted by 24.
- `uniprot-P04439.json`: original UniProt annotation snapshot with version metadata.
  UniProt's displayed sequence is A*03:01; only shared region boundaries are used.
- `hla_a_sources.tsv`: source URLs and SHA-256 values, including the official
  alignment used to validate coordinate correspondence.
- `load-hla-a-example.R`: all residue states, per-substitution counts, and up to
  three example names for each state.
- `plot-hla-a-example.R`: bounded symmetric fanning, log10(count + 1) heights,
  chemical-group colours, and the labelled residue-156 close-up.
- `ipd-HLA00005-reference.json`: original release-3.66.0 reference snapshot,
  including genomic/coding/protein sequences and exon/intron coordinates.
- `hla_a_reference_genomic.fasta`, `hla_a_reference.gff3`, and
  `hla_a_gene_structure.tsv`: reference sequence and derived coding model.
  The display spans ATG to stop, with eight coding exons and seven introns.
- `project-hla-a-example.R`: constructs a SynIndividual, attaches the summary
  annotation, and projects precursor residues through the reference CDS.
  Splice-split codons anchor to an exonic fragment. The scale compresses introns
  three-fold, with separate scale bars; base-edge exon geometry preserves exact
  bp lengths. Other alleles are projected to this one reference model.

All 1,212 substitutions are retained in the dense plot. Fanning is bounded
locally, preventing cumulative displacement beyond the protein; stem anchors
retain their true residue positions. Some nearby equal-count heads may overlap.

At residue 156, counts are L=3172 (reference), F=2, M=1, Q=1184, R=596, W=1043.

## Reproduction

Run from the repository root:

```sh
python3 data-raw/hla_a_lollipop/build-hla-a-demo.py
Rscript data-raw/hla_a_lollipop/verify-hla-a-demo.R
Rscript data-raw/hla_a_lollipop/verify-hla-a-gene-model.R
Rscript data-raw/hla_a_lollipop/render-hla-a-figure.R
```

The builder downloads pinned sources into `~/.cache/ggexon/hla_a_lollipop`.
Running the R example requires no network. PNG/PDF figures and summary TSVs
are exported under `data-raw/hla_a_lollipop/preview`.

## Attribution and terms

Original IPD records retain `IPD-LICENCE.md` and `release_version.txt`. The package
MIT license does not supersede source terms. Please cite:

- Barker DJ et al. The IPD-IMGT/HLA Database: recent developments in sequence
  submission. Nucleic Acids Research (2026), 54(D1): D1152–D1158.
- Robinson J, Barker D, Marsh SGE. 25 years of the IPD-IMGT/HLA Database.
  HLA (2024), 103(6): e15549.
- Robinson J et al. IMGT/HLA - a sequence database for the human major
  histocompatibility complex. Tissue Antigens (2000), 55:280–287.

UniProt annotation is attributed to the UniProt Consortium under CC BY 4.0:
https://www.uniprot.org/help/license.
