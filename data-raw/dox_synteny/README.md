# Reproduce the compact Dox example

The offline input bundle is in `inst/extdata/dox_synteny/`. It compares
D. mauritiana, D. simulans, and D. sechellia at the Dox insertion locus
illustrated in [Vedanayagam et al. (2021), Extended Data Figure 5](https://pmc.ncbi.nlm.nih.gov/articles/PMC8665063/#F11).
There are three species groups, each with separate RNA/annotation subpanels,
two nucleotide-link panels, and 2/3/2 displayed genes. Annotation : link : RNA
heights are 1 : 0.5 : 0.5. The figure demonstrates the new
`facet_genomics(coverage_panel_height = ...)` argument and existing independent
RNA y limits; it requires a ggexon version with that argument.

Run from the repository root:

```sh
# Prepare Dsim RNA: downloads ~3 GB of reads plus the pinned reference genome.
# Requires Python 3, minimap2 >=2.29, samtools, and internet.
python3 data-raw/dox_synteny/remap-dsim-rnaseq.py --download

# Rebuild the small bundle using the verified remapping and public source windows.
# Also requires LASTZ.
python3 data-raw/dox_synteny/build-dox-data.py

# Convert verified tables into native GTF/PAF/BigWig tutorial inputs.
# Requires rtracklayer on macOS or Linux; values and coordinates are retained.
Rscript data-raw/dox_synteny/prepare-dox-objects.R

# Execute the tutorial's actual object-first code and render all stages offline.
# Requires the package's R dependencies, knitr, rtracklayer, and ragg.
Rscript data-raw/dox_synteny/render-dox-example.R

# Verify the fixture, exact CDS reconstruction, alignments, and plot layout.
Rscript -e 'devtools::test(filter = "dox-synteny-demo-data")'
python3 data-raw/dox_synteny/test-remap-dsim-rnaseq.py
```

`figures/` contains PNG and vector PDF versions of the annotation-only stage,
the annotation/linkage stage, and the complete RNA figure at original genomic
scale and with gaps compressed three-fold. The renderer extracts runnable
chunks from the tutorial into a fresh environment, checks 3/5/8 panel roles,
row heights, visible intervals, and RNA limits, and exports those exact plots.
It also updates `man/figures/dox-three-species-demo.png`. Descriptions and
source details are in the tutorial text rather than a figure caption.

The builder stages outputs before copying them into the bundle. Its ignored
`downloads/` cache holds public source responses and verified BigWig byte
ranges. `manifest.json` records source URLs, byte-range hashes, aligner
settings, and bundled data checksums.

Two source details are essential when reusing the example:

- The Dox coding model is derived from the published 474-nt CDS, mapped
  uniquely and perfectly to three coding exons on the selected assembly.
  It does not reconstruct UTRs or a full-length transcript. The flanks are
  UCSC GenArk mappings of common D. melanogaster RefSeq transcript models.
- D. mauritiana and D. sechellia retain deposited replicate-1 testis RNA-seq
  scores. D. simulans uses the complete original replicate-1 library,
  SRR23418969 (40,381,684 paired-end read pairs), remapped to the exact plotted
  assembly. Its incompatible deposited BigWigs remain excluded. No coordinate
  offset is inferred. Dsim is raw read depth, whereas the outer species retain
  deposited signal units; independent y scales are used for visualization.

The remapping script verifies all three compressed input files against their
public MD5 checksums and the full decompressed genome SHA256. It validates the
entire BAM chromosome dictionary, complete-library read count, and the plotted
43,781-bp sequence. It retains primary alignments with MAPQ >=20, assigns RNA
strand for a verified reverse-stranded paired library, and excludes intron and
deletion bases from depth. Overlapping mates each count as read depth. A second
implementation (`samtools depth`) must agree at every plotted base.

To audit an existing BAM from the same run without repeating alignment, use
`python3 data-raw/dox_synteny/remap-dsim-rnaseq.py --reuse-bam`. All validation
still runs. The builder consumes `downloads/remapping/Dsim-remapped-coverage.tsv.gz`
and copies `Dsim-remapping-qc.json` into the offline bundle. Large FASTQs,
reference, index, and BAM remain ignored by Git.

See `vignettes/dox-synteny-demo.Rmd` for the plotting grammar and source links.
