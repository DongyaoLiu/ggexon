# HOXA Ensembl 115 Data Preparation

Run `build-hoxa-demo.R` from the repository root to regenerate the bundled
HOXA/Hoxa demo tables:

```r
Rscript data-raw/hoxa_ensembl115/build-hoxa-demo.R
```

The script downloads Ensembl release 115 GTF files, extracts gene rows whose
normalized `gene_name` matches `HOXA[0-9]+`, orients display coordinates so all
species read HOXA13-to-HOXA1, and writes the small derived TSV files under
`inst/extdata/hoxa_ensembl115/`.

It also writes a combined `hoxa_homology.tsv` table for `HomologyAnnotation()`
and tiny original-coordinate GFF3 files under
`inst/extdata/hoxa_ensembl115/annotations/` so examples can build a real
`SynSpecies` object without shipping full Ensembl GTFs.

Set `GGEXON_HOXA_CACHE=/path/to/cache` to reuse downloaded GTF files between
runs. Downloaded GTFs are not bundled with the package.
