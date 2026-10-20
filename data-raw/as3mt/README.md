# AS3MT transcript architecture and neural phenotypes

Two reproducible figures based on the user-supplied `AS3MT_plotting_handoff.zip`.
The extracted inputs are preserved in `handoff/`; the original specification is
`handoff/as3mt_plot_spec.json`.

## Outputs

- `figures/AS3MT-exon-skipping-neural-phenotypes.pdf` and `.png`: requested
  three-panel figure. A is a schematic alignment by reference exon number;
  B uses canonical amino-acid coordinates; C shows all deposited percentages
  with arithmetic mean and sample SD / sqrt(n) SEM.
- `figures/AS3MT-transcript-pairwise-genomic.pdf` and `.png`: CD44-style
  human–chimpanzee comparison in one full-locus view (no second zoom), built with
  `SynSpecies`, `SynIndividual`, `SynPairAlignment`, `geom_exon2`,
  `geom_nuclink`, `facet_genomics`, `scale_x_ggexon_genomic(intron_factor = 30)`
  and `guide_x_ggexon_piecewise(by = "panel")`. Exons retain their native
  widths; introns are drawn at one-thirtieth their genomic length. The scale
  uses the union of exon intervals within each assembly panel, keeping both
  human transcript rows aligned. Separate exon/intron rulers make the two
  scales explicit. E2/E3 are gold in the human full-length row and dashed
  outlines in the reconstructed row; original exon numbers are retained.
  Ribbons are actual LASTZ genomic alignments; they are not
  expression measurements or exon-number correspondence links.
- `figures/AS3MT-transcript-pairwise-nogap.pdf` and `.png`: explicitly named
  copies of the updated pairwise figure, now using ungapped LASTZ blocks.
- `as3mt-comparison.rds`: reusable comparative ggexon object.
- `compressed-scale-bars.tsv`: exact genomic and display widths used by the
  native scale guide; the script checks their 30-fold intron ratios.
- `organoid-reported-values.tsv` and `organoid-summary.tsv`: plotted data and
  independently recalculated summaries.

## Rebuild

Run from the ggexon repository root:

```sh
python3 data-raw/as3mt/prepare-alignment.py
Rscript data-raw/as3mt/render-figures.R
Rscript data-raw/as3mt/bundle-inputs.R
```

The Python step requires LASTZ and network access. It validates the assembly
names before downloading chromosome-10 windows from Ensembl. GRCh38.p14's
primary chr10 uses the handoff's GRCh38 coordinate system. Downloaded windows,
command, LASTZ version, and SHA-256 digests are recorded in
`sequence-provenance.nogap.json`. The R step can run offline once those files exist.
Its presentation parameter block controls dimensions, colours, fonts and sizes.
PDFs use vector Cairo output at 180 mm width; PNGs are previews.

To reuse the exact downloaded windows, run the preparation step with
`--reuse-windows`; it checks their saved sequence hashes. The updated command
uses `--nogapped --chain --strand=both`, emitting one gap-free block per PAF
row. Chaining selects blocks without merging them into gapped alignments.
Mismatches are allowed. Every raw block is checked for a single `lengthM`
CIGAR, equal query/target/alignment spans, and a matching-base count that agrees
with the underlying forward or reverse-complement DNA. Filtering retains
blocks ≥80 bp and ≥50% identity. Raw output is `human_chimp.nogap.paf`;
filtered chromosome-coordinate links are `human-chimp-links.nogap.tsv`.
The plotted links are restricted to the two highest identity categories
(97.1–98.3% and 98.3–100.0%, labels rounded to one decimal). These are the
original quantile boundaries from all retained blocks; they are not recomputed
after filtering. The plotted genomic subset is
`human-chimp-links.nogap.top2.tsv`; lower categories are also removed from the
legend. The full ungapped alignment remains available.
The previous gapped `human_chimp.paf`, `human-chimp-links.tsv` and
`sequence-provenance.json` remain available for comparison.

The pairwise drawing now uses native automatic link scaling in ggexon.
`geom_exon2(compress_introns = FALSE)` avoids applying a second schematic
compression. Half-base box edges preserve inclusive exon lengths. The plot
passes raw genomic links from the stored alignment directly to `geom_nuclink()`.
`scale_x_ggexon_genomic()` subdivides each link at the union of exon/intron
scale boundaries in its two source panels, then applies those panels' maps.
Stored genomic windows are automatically converted into display limits.
There is no AS3MT-specific coordinate adapter. Original genomic inputs remain
unchanged. For this plot, 37 original ungapped blocks generate 57 polygon pieces.
`native-link-pieces.tsv` records the drawing vertices, their original genomic
positions, and parent-block identifiers. Scale-bar ratios are checked on export.

## Biological and statistical interpretation

The reconstructed human transcript retains full-reference E1 and E4–E11 and
canonical reference ends. It has no independently established stable accession.
Its 2,281-nt length is conditional on those ends and the reference repeat allele.
Skipping removes 169 RNA nucleotides, destroys the original E1/E2-split ATG,
and uses the handoff-assigned downstream start in E4. It is **not** a 306-nt
in-frame exon deletion. The assigned protein has 273 residues, corresponding
to full-length residues 103–375. The script verifies full and reconstructed
cDNA against the downloaded genomic sequence and translates both assigned CDSs
to verify the supplied protein sequences.

Panel C compares control with AS3MTd2d3 overexpression in H9/WA09 human
forebrain organoids, not humans with chimpanzees. Each endpoint has 7 control
and 11 overexpression reported values. The publication does not define the
sampling unit; these counts must not be called independent donors or organoids.
P = 0.0008 (SOX2) and P = 0.0014 (TUJ1) are the **reported** two-sided
unpaired-t-test P values, not tests reconstructed from rounded table values.
The SEM uses the supplied percentages and is checked against the specification.
Points are spread deterministically along x solely to show overlapping values;
their order has no experimental meaning. No pairing is asserted.

The chimp E1–E4 junction was not detected in the examined Li et al. RNA-seq;
this does not establish absence in all chimpanzee tissues. The short human
isoform is also expressed in healthy humans. No species-specific disease-rate
claim is made. Optional rat-neuron and spindle-angle panels are not included.

## Sources

- Li et al. (2016), *Nature Medicine*, doi:
  [10.1038/nm.4096](https://doi.org/10.1038/nm.4096).
- Cai et al. (2021), *Schizophrenia Bulletin*, doi:
  [10.1093/schbul/sbaa098](https://doi.org/10.1093/schbul/sbaa098), Fig. 1 minigene assays.
- Kim et al. (2025), *Science Advances*, doi:
  [10.1126/sciadv.adp8271](https://doi.org/10.1126/sciadv.adp8271),
  Supplementary Fig. S8B–C and Table S5, cells A34:A40 and B34:B44.
- Assembly-specific selected Ensembl release-116 and RefSeq annotations,
  accession versions, original source snapshots and evidence caveats are
  preserved in the handoff JSON and `handoff/sources/`.

The example requires this checkout's native piecewise link-scaling support.
The bundling step refreshes the portable installed inputs for the pkgdown
tutorial. The example helper is `inst/extdata/as3mt/load-as3mt-example.R`.

## Anchored DNA differences

`prepare-alignseq.R` expands the original longest forward `human_chimp.paf`
CIGAR against the two offline FASTAs, checks genomic endpoints and the PAF
matching-base count, and writes `inst/extdata/as3mt/aligned-dna.rds` plus an
inspectable `alignseq-events.tsv`. Run it with the development package loaded.
The payload records the full PAF row and input FASTA MD5 hashes.

The gapped payload retains the earlier generic E2/E3 illustrative selection
for audit. It is not used to infer the complete VNTR: the longest LASTZ block
begins within the human repeat array. `render-alignseq.R` now renders the
VNTR-focused feature below, as vector PDF and PNG. Each species keeps its
own DNA callout on its own compact annotation panel, with smooth anchored
connectors and two calibrated exon/intron bars in the external legend.

## Functionally supported exon-1 VNTR (current sequence figure)

`prepare-vntr.R` reads the supplied `genomic_VNTR` specification, checks all
three 36-bp human GRCh38 units against `human.fa`, and extracts the chimp
imperfect homolog using its independently annotated E1 donor. It saves
`inst/extdata/as3mt/vntr-dna.rds`, `vntr-repeat-units.tsv` and explicitly labelled
motif-level two/three-repeat reconstructions. No full experimental plasmid
sequences or new expression measurements are implied.

The human terminal unit is 102869570–102869605; the E1 donor is after 102869593.
The chimp homolog is 104324211–104324246; donor after 104324234. Both have 24
exonic plus 12 intronic bases and a conserved GT donor. The donor-anchored
comparison puts 72 gap columns opposite the other two human copies; equivalent
placements among identical copies are not evolutionarily distinguishable.

Cai2021 minigenes establish repeat-dependent regulation (three repeats produce
more d2d3 than two) in the tested cells. The figure distinguishes this human
allele contrast from the human/chimp reference comparison. Evolutionary
sufficiency of the expansion is not established. Original LASTZ audit data
remain unchanged; only the sequence feature used in the figure is replaced.
