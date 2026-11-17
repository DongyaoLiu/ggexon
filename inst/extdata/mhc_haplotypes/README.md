# Three-haplotype MHC example

This offline example compares Siamang H1 (`SSYh1`), Gorilla H1 (`GGOh1`) and
Human CHM13 (`HSAh1`). It contains 58 curated functional MHC genes and 443
representative background models. Tutorial 04 prepares a `SynSpecies`, its
feature annotations, two direct alignments, reusable gene views and layouts
before drawing gene spans, labelled gene-lollipop heads and nucleotide ribbons.

## Files and coordinates

- `windows.tsv` lists the three native chromosomes, assemblies, equal-sized
  windows, display orientation and sequence sources.
- The three `.gff3` files contain one gene feature per retained curated MHC
  locus or representative background model, with source names, roles and
  colours. They contain gene spans, not reconstructed exon structures.
- The two `.paf` files store direct adjacent comparisons in native assembly
  coordinates. `SSYh1_GGOh1` connects Siamang H1 to Gorilla H1;
  `GGOh1_HSAh1` connects Gorilla H1 to Human CHM13.
- `genes.tsv` and `links.tsv` retain the plotted source intervals and alignment
  block provenance. `manifest.json` records source versions, filtering rules,
  sequence and alignment hashes, tool version, commands and data-file hashes.
- `prepare-mhc-example.R` prepares the grey native-span background view from
  attached feature annotations. The tutorial uses `prepare_gene_lollipop()`
  for MHC genes and shared row/column slots, and `query_gene_lollipop()` to
  inspect them. The geom consumes the prepared grid through Syn dispatch.

GFF3 and TSV coordinates are one-based and inclusive; PAF starts are zero-based
and are converted by the package importer. All coordinates and strands remain
native. Siamang is displayed with `facet_genomics(reverse_x = "SSYh1")`.
No manual reflection or coordinate offset is applied during plotting.

Each window is 3,628,206 bp: the longest functional MHC extent among these
three haplotypes plus 200 kb. Windows are centred on each track's functional
MHC extent. This trims the earlier outer flanks while preserving all 58
functional MHC records. Equal panel widths have the same base-pair scale.

## Data provenance and interpretation

The source study is [Yoo et al. (2025), Complete sequencing of ape
genomes](https://doi.org/10.1038/s41586-025-08816-3).
Ape MHC gene spans and functional-status calls come from the
[T2T browser MHC annotations](https://github.com/marbl/T2T-Browser/tree/603f414e9278545a1c8dcf416f3226e7a3d07e86/src/MHC),
pinned to browser commit `603f414e9278545a1c8dcf416f3226e7a3d07e86`.
Human MHC spans come from CHM13 RefSeq Liftoff v5.2, collapsed by symbol,
chromosome, strand and status. Functional MHC counts are 18 in Siamang H1,
21 in Gorilla H1 and 19 in Human CHM13. Functional is a source annotation
category, not experimental validation.

Background genes use released CAT/RefSeq consensus v3 models for the two ape
H1 assemblies and CHM13 RefSeq Liftoff v5.2 for human. One transcript with the
longest total exonic length represents each overlapping same-name, same-strand
locus. The selected haplotypes use released context annotations; no alternate
haplotype projection is needed. Retained background counts are 138, 121 and
184 respectively. These counts have different annotation ascertainment.

Curated MHC pseudogenes are omitted. Background filtering removes explicit
pseudogene biotypes, background symbols called pseudogene in the CHM13
annotation, and overlapping source models of curated MHC loci, including
their pseudogenes. The latter rule prevents excluded MHC pseudogenes from
reappearing as grey background arrows. CAT does not supply a biotype in these
source tables, so its other background records retain `gene_type=unspecified`.
The reference-symbol exclusion is a display choice; it does not establish
functional status in either ape. All exclusions are auditable in
`data-raw/mhc_haplotypes/excluded-models.tsv`.

MHC arrows and labels retain the original browser ortholog colours; background
genes are grey. Variable-region bars and MHC cluster underlines are omitted.
Missing or filtered labels do not establish biological gene absence.

`geom_gene_lollipop()` draws an enlarged directional head for every retained
MHC gene, with its label inside and a leader anchored at the true midpoint of
the native span. The small native arrows remain below the heads. Four shared
head rows keep matching source keys aligned across haplotypes; empty slots
are display space, not absence calls. Head size and spacing use physical
millimetres, and text is measured on the output device.

The regional DNA is extracted directly from the assembly 2bit URLs in
`windows.tsv` using bounded HTTP requests. Sequences are retained in the
external build cache, not required for plotting. Both adjacent alignments
are computed directly with minimap2 `asm20`, CIGAR output and no secondary
records. Alignment envelopes split at indels of at least 1 kb; both source
spans must be at least 500 bp and block identity at least 70%. Short internal
indels remain within the envelopes. The exported PAF mapping quality is 255
(unknown). These ribbons are reconstructed alignments of the selected
assemblies, not recovered plot data from the published figure.

## Rebuild

The bounded source tables under `data-raw/mhc_haplotypes/source/` were selected
from repository commit `eabd8aa`; their manifest records the full commit and
original table hashes. They retain only the three requested haplotypes.

From the repository root:

```sh
python3 data-raw/mhc_haplotypes/build-mhc-example.py
Rscript data-raw/mhc_haplotypes/render-mhc-example.R
```

The builder needs internet access and minimap2 when the external sequence and
alignment cache is empty. Rendering uses only the bundled files and executes
the actual vignette code. It exports all three stages as PNG and vector PDF,
and records the physical head layouts. Checks require all 58 lollipop heads,
unchanged native anchors, correct reversed-Siamang directions, shared rows,
label fit and panel bounds on both output devices.
