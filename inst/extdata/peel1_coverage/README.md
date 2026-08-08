# PEEL-1 coverage fixtures

This directory contains raw coverage BigWig fixtures for four *C. elegans*
strains: `XZ1516`, `ECA2091`, `ECA701`, and `ECA2191`.

The tracks cover chromosome I, interval `2332338-2373985` (1-based,
inclusive), with one-base bins and no normalization.  The accompanying GTF is
a WS285 canonical-gene-set subset containing exactly these four genes:

- `WBGene00021464`
- `WBGene00021463`
- `WBGene00077563`
- `WBGene00021461`

## Provenance and rebuild

The GTF source is the *C. elegans* PRJNA13758 WS285 canonical geneset.  The
BigWigs are generated from indexed BAMs supplied through
`GGEXON_TA_BAM_DIR`; set `GGEXON_WS285_GTF` to the source GTF and
`GGEXON_BAMCOVERAGE` to the executable. The manifest reports the intended
1-based inclusive interval. The builder passes `I:2332337:2373985` to
`bamCoverage` because that command's region start is zero-based; this produces
the exact inclusive BigWig span `I:2332338-2373985` (41,648 bp). Rebuild from
the package root with `bamCoverage` 3.5.6:

```sh
GGEXON_TA_BAM_DIR=/path/to/bams \
GGEXON_WS285_GTF=/path/to/c_elegans.PRJNA13758.WS285.canonical_geneset.gtf \
GGEXON_BAMCOVERAGE=/path/to/bamCoverage \
Rscript data-raw/peel1_coverage/build-peel1-coverage-fixtures.R
```

`manifest.tsv` records the source BAM path, output filename, interval,
binning, normalization, `bamCoverage` version, and MD5 checksum for every
track.
