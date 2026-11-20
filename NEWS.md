# Development version

* Plot biallelic population allele frequencies along linkage ribbons with
  `geom_allele_frequency()`. Import MUMmer SNP correspondences with
  `SynSNPAnnotation()`, match them to a reference VCF with
  `prepare_allele_frequency()`, and inspect matches and exclusions with
  `query_allele_frequency()`.

# ggexon 1.0.0

First stable release.

1. **Syn objects:** Store genomes, annotation layers, alignments, homology mappings, and panel layouts in `SynIndividual` and `SynSpecies` objects, then let plotting layers retrieve the relevant data.
2. **Genomic and protein layers:** Combine gene models, DNA alignments, RNA-seq coverage, splice junctions, protein features, and mutation summaries in ggplot2 figures.
3. **Reusable preparation:** Prepare shared genomic projections, intron compression, and gene-lollipop grids on Syn objects before plotting or subsetting.
4. **Panel layouts:** Arrange annotation, coverage, and alignment panels with independent genomic windows, grouped individual labels, and panel-specific height controls.
5. **Tutorials:** Follow the object-to-plot workflow through basic usage and four complete examples covering HLA-A, AS3MT, Dox synteny, and three MHC haplotypes.
6. **Citation and installation:** Install the tagged `v1.0.0` release and retrieve the package citation and published PLOS Biology article with `citation("ggexon")`.

BigWig input uses rtracklayer and is supported on macOS and Linux; ODGI-backed graph alignment workflows require Python 3.8 or newer and `odgi`.
