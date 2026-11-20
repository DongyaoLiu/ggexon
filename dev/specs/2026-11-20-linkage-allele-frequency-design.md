# Allele frequencies in linkage panels

The user approved implementing this design after a one-question-at-a-time
design interview. Existing unrelated edits remain in the original checkout.

## Behavior

- Plot biallelic SNPs only: one A/C/G/T REF, one different A/C/G/T ALT, one AF.
- Read precomputed population frequency from configurable VCF INFO/AF by
  default. Preserve the VCF source and metadata; do not compute genotypes.
- Import MUMmer nucmer show-snps correspondence upstream, without requiring
  genome FASTA access during preparation or plotting. Accept the documented
  tabular, sequence-length-containing layout produced by show-snps -ClrTHI.
  Headered output and unfiltered output with overlap-count columns may also be
  accepted when their layouts can be identified unambiguously.
- Join by reference contig/position and REF/ALT identity, after normalizing
  MUMmer bases into reference-forward orientation. Query positions remain
  native, forward-genome coordinates, including on inverted alignments.
- Keep only uniquely mapped matching REF/ALT SNPs. Exclude and report missing,
  ambiguous, incompatible, non-SNP, multiallelic, and invalid-frequency records.
  An absent MUMmer record does not establish a REF match. No 1-minus-AF allele
  conversion is used for these difference-only data.
- MUMmer SNP correspondence supplements an existing named pairwise alignment;
  it does not become a new ribbon-producing alignment format.
- Store named preparations on SynSpecies and return the updated object.
  An inspection verb returns matched rows, excluded rows, or their summary.
- Keep native coordinates in stored data. At draw time transform each endpoint
  through its own annotation panel, then interpolate both displayed x and y.
  AF=0 is the reference edge, AF=1 the nonreference edge, AF=.5 the midpoint.
  Panel reordering and the stored alignment's query/target order must not
  change that meaning. Point size, shape, colour, fill, alpha, and stroke use
  normal ggplot2 point semantics.

## Object and verbs

SynSNPAnnotation extends SynSpeAnnotation and records source_file,
reference_individual, query_individual, alignment, and normalized snps.
SynSpecies gains a named SNP annotation collection through snp_annotations()
and the validated setter verb add_snp_annotation(). Store it under a private
metadata key rather than changing the S4 schema, preserving the validity of
previously serialized SynSpecies objects. The mapping remains reusable across
VCF datasets.

prepare_allele_frequency(x, vcf, snps, name = "allele_frequency",
af_field = "AF") consumes named stored sources. The VCF belongs to the named
reference individual. Validate that the SNP source and stored pairwise
alignment identify the same two individuals. Cache derived rows on the SNP
annotation's existing plot_cache; store a small name/source registry in
SynSpecies metadata. Source replacement, source-file changes, and cleared
caches require preparation again, with an actionable error.

query_allele_frequency(x, name = NULL,
component = c("variants", "excluded", "summary")) inspects the result.
Infer a name only when exactly one preparation exists.

geom_allele_frequency(mapping = NULL, data = NULL, preparation = NULL, ...)
uses LayerSyn and the existing link-panel layout. Explicit prepared-style
tables also work. It inherits point aesthetics and delegates point rendering
to GeomPoint after computing displayed coordinates. Missing or ambiguous
preparations and missing link/source panels give explicit errors.

## Implementation boundaries

Keep the source importer, preparation, and geom in separate small R files.
Use existing VCF and alignment accessors. A new source annotation is justified
because it records imported biological correspondence independently of any
display. A second VCF container or a biological class solely for prepared
display coordinates would duplicate existing responsibilities.

## Verification

Use literal tiny VCF and MUMmer fixtures. Cover reverse strand/base orientation,
unique versus duplicated mappings, single ALT enforcement, AF errors, retained
exclusion reasons, original-source preservation, named dataset ambiguity,
source replacement, and source/target order. Render link-panel points with
AF=0, .25, .5, 1 and compare their actual grob locations with independently
derived endpoints. Include genomic compression and panel reordering, then run
the package suite and document an end-to-end reproducible example.


## Verified implementation

Implemented through runtime commit 9af56ee1792b4995ce1f028c466bcfe6de24acc8.
The public import, preparation, inspection and plotting APIs are exported and
documented. The preparation preserves the existing VCF query API's eight fixed
columns; the original full VCF file is retained. Its source-row numbers index
queried records on the SNP source's reference chromosomes.

Both implicit SynSpecies plotting and explicit prepared tables are supported.
Track labels are arbitrary, and a ribbon plus frequency overlay shares one
linkage panel. Fixed per-SNP point styles remain attached to their original
SNPs through window selection and endpoint expansion.

Independent review approved the final fixes. Verification passed 5,244 full
source assertions and 410 fresh installed focused assertions, with no test
failures, warnings or skips. The final package check passed installation,
code, documentation and examples with zero errors, zero warnings and three
package-level notes. The final check skipped its duplicate test run; the
standalone full source and installed focused suites supplied that evidence.

As with ordinary clipped points, glyphs centred on AF=0 or AF=1 are bisected
by the linkage-panel edge. Their centres retain the exact specified positions.
The executable example documents this behavior.
