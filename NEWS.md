# ggexon 1.0.0

First stable release.

1. **Syn objects:** Store genomes, annotation layers, alignments, homology mappings, and panel layouts in `SynIndividual` and `SynSpecies` objects, then let plotting layers retrieve the relevant data.
2. **Genomic and protein layers:** Combine gene models, DNA alignments, RNA-seq coverage, splice junctions, protein features, and mutation summaries in ggplot2 figures.
3. **Reusable preparation:** Store nucleotide alignments, shared genomic projections, intron compression, and gene-lollipop grids on Syn objects for plotting and inspection.
4. **Panel layouts:** Arrange annotation, coverage, and alignment panels with independent genomic windows, grouped individual labels, and panel-specific height controls.
5. **Tutorials:** Follow the object-to-plot workflow through HLA-A, AS3MT with transcript-attached sequence connectors, Dox synteny, and three MHC haplotypes.
6. **Citation and installation:** Install the tagged `v1.0.0` release and retrieve the package citation and published PLOS Biology article with `citation("ggexon")`.

BigWig input uses rtracklayer and is supported on macOS and Linux; ODGI-backed graph alignment workflows require Python 3.8 or newer and `odgi`.
