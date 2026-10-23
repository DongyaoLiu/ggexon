# Immune Locus Atlas Implementation Plan

**Goal:** Focus this isolated branch on immune loci from Extended Data Figs2and3, retaining integrated below-gene annotations and producing separate locus plots and one combined multifacet overview.

**Scope:** MHC classI/classII plus IGH,IGK,IGL,TRA/TRD,TRB,TRG. TRA/TRD occupy one shared genomic facet. Compare a common human haplotype plus the two source haplotypes of four nonhuman ape species. Counts derive from source calls, not published average targets.

- [x] Remove the obsolete nonimmune examples, their source inputs, generation branches and result files from the isolated worktree.
- [x] Acquire source IG/TR gene calls, alignments, SV/SD blocks, locus bounds and functional/species-specific labels, preserving raw metadata and coordinate correction evidence.
- [x] Acquire full curated MHC classI/classII gene status and coordinate data, preserving ascertainment caveats and human display scope.
- [x] Validate inputs and build a matrix of common species/haplotypes by genomic region, with annotations below gene markers and nucleotide links in separate rows.
- [x] Export separate locus figures and a combined overview containing gene organization/counts and available structural annotations; preserve the MHC DR detail with methylation.
- [x] Validate counts/coordinate conversions, test panel identity and locus-local links, inspect every PDF preview and document source limitations.
