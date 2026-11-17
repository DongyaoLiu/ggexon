# Offline AS3MT example inputs

Source `load-as3mt-example.R` and `as3mt-object-data.R`. `load_as3mt_example()`
returns one `SynSpecies` containing human/chimpanzee annotations, the selected
alignment. Run `prepare_as3mt_example()` once to attach named views for exon
labels, aligned DNA, repeat outlines and donor marks. The tutorial shows the
plotting commands directly from `ggexon(as3mt)`.
`as3mt_evidence()` reads original evidence, and `as3mt_plot_data()` reads a
prepared view. Existing figure-export helpers remain available.

`human-rnaseq-GTEX-15G19.bw` is a small BigWig re-encoding of the verified TSV
snapshot, rebuilt by `data-raw/as3mt/prepare-coverage-bigwig.R`. Every interval
and score is preserved, including explicit zeros; missing intervals remain
missing. `geom_coverage(species = "human", annotation = "gtex_ba9")` reads it
through native Syn dispatch on the loaded object. This total signal combines
transcripts and strands and cannot be divided into isoform-specific coverage.
`geom_exon2(species = c("human", "chimp"))` selects the annotation panels.

`handoff/` preserves the supplied plotting specification, source annotation
snapshots, original percentage values and biological interpretation caveats.
`specification.rds` and `alignment-provenance.rds` are portable R serializations
of the corresponding JSON documents, avoiding a JSON dependency at render time.

The ungapped LASTZ alignment has 124 raw blocks, 93 after the documented
length/identity filter, and 37 in the two highest original identity categories.
The selected 37 are stored in the comparative object using their original
one-based closed chromosome coordinates. PAF uses zero-based half-open local
window coordinates. The loader uses the already converted genomic TSV.

Human assembly: GRCh38; chimpanzee: Pan_tro_3.0. Never put their genomic
coordinates on a shared linear axis. `geom_exon2()` uses thick CDS and thin UTR;
`scale_x_ggexon_genomic()` preserves exon widths and compresses introns. Native
`geom_nuclink()` subdivides links at both source panels' scale boundaries.

The short human transcript is a paper-defined reconstruction, not a stable
accession. The organoid sampling unit is not defined by the publication; n=7/11
counts reported values, not established independent donors or organoids.

The legacy `plot_as3mt_coverage()` and combined `plot_as3mt_alignseq()` exports use matched
GTEx BA9 total-coverage and splice-junction snapshots. `geom_junction()` draws
counted inclusion and E1-to-E4 skipping arcs on the human coverage panel by
default. Set `junction_panel = "annotation"` in either helper to use the gene
model instead. The geom's own `panel` argument selects its target; `y`, `yend`
and `height` use that panel's units. Coverage arcs meet the unchanged signal at
the donor and acceptor exon bases. Staggered label heights separate the narrow
compressed introns; curve rise is a layout choice, not another count encoding.
These short-read events do not quantify complete isoforms or establish the
independent donor's VNTR genotype. The GTEx provenance JSON records source
URLs, assembly, coordinate conventions and verification. The previous ENCODE
snapshot remains bundled for reproducibility.

Sources: Li et al. 2016, doi:10.1038/nm.4096; Kim et al. 2025,
doi:10.1126/sciadv.adp8271. See the tutorial and source specification for details.
