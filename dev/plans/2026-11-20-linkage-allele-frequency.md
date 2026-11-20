# Linkage allele-frequency implementation plan

> For agentic workers: use subagent-driven-development when delegating these
> tasks, with task-scoped review and a final branch review. Follow test-driven
> development and verification-before-completion. The user has authorized
> implementation; no additional execution-choice approval is needed.

**Goal:** Store MUMmer SNP correspondences, prepare matching VCF AF records on a
SynSpecies, and plot frequency-interpolated points in existing linkage panels.

**Architecture:** A species-level SNP source annotation is separate from VCF
sources and ribbon alignments. Preparation stores native paired coordinates;
the geom transforms endpoints through their source panels at draw time.

**Tech stack:** R, S4, ggplot2 4.x ggproto, LayerSyn, testthat, roxygen2, grid.

## Global Constraints

- Only biallelic SNPs with one distinct REF and ALT A/C/G/T base are eligible.
- Precomputed AF comes from configurable INFO/AF and is finite within [0,1].
- Only unique MUMmer REF/ALT matches are plotted; no REF-only 1-minus-AF case.
- No FASTA requirement or external MUMmer invocation at plot time.
- MUMmer supplements existing PAF/PSL/ODGI pairwise ribbon alignments.
- Native genomic coordinates remain inspectable and immutable during plotting.
- Return updated SynSpecies from preparation; preserve original VCF sources.
- Do not modify unrelated original-checkout edits or introduce dependencies.

## Task 1: Source annotation and preparation

Files: create R/snp-annotation.R, R/allele-frequency.R,
tests/testthat/test-allele-frequency-data.R; modify DESCRIPTION for Collate
entries. Store the collection in private SynSpecies metadata, accessed through
snp_annotations(), to preserve existing serialized S4 objects. Generated exports and
Rd files are finalized in Task 2.

Interfaces produced:

    SynSNPAnnotation(name, snp_file, reference_individual, query_individual,
                     alignment, snps = NULL, metadata = list())
    add_snp_annotation(x, annotation)
    snp_annotations(x)
    query_snps(x, name = NULL, chr = NULL, start = NULL, end = NULL)
    prepare_allele_frequency(x, vcf, snps, name = "allele_frequency",
                             af_field = "AF")
    query_allele_frequency(x, name = NULL,
                          component = c("variants", "excluded", "summary"))

SynSNPAnnotation extends SynSpeAnnotation; normalized snps fields are
reference_chr, reference_pos, reference_base, query_chr, query_pos, query_base,
strand. Both bases are reference-forward, both positions forward-genome
one-based coordinates. Optional overlap columns flag ambiguous MUMmer rows.
The source's reference/query can match either orientation of the named stored
pairwise alignment. The prepared table retains the existing VCF reader's eight fixed columns plus
source_row, af, tspecies, tchr, tstart, tend, qspecies, qchr, qstart, qend,
strand, track, group, reference_is_target. t/q follow the stored alignment;
reference_is_target preserves the user-defined AF direction. track is the
stored alignment's actual track or its format-specific standard link label.

Store full prepared records and exclusion data under the SNP annotation's
plot_cache$allele_frequency[[name]], with a small species metadata registry.
Use rlang::hash and source file size/mtime/path signatures to invalidate
replaced sources and VCF/SNP file changes. Check current source identifiers,
alignment identity, and cached payload validity during inspection/plotting.

- [x] Write literal VCF and show-snps fixtures and failing tests. A forward
  A>G SNP at reference 101 and query 901 with AF=.25 must retain those exact
  positions and AF; a reverse query frame must preserve query position and
  leave the already reference-oriented show-snps G base unchanged. Explicitly
  test a reverse reference frame by complementing both reported bases once.
- [x] Run devtools::test(filter="allele-frequency-data") and observe missing
  API failures before writing production code.
- [x] Implement source import, validation, collection attachment and query.
  Accept canonical show-snps -ClrTHI 12-column tabular rows. Handle header
  lines without mistaking file paths for data. Reject unsupported layouts with
  an actionable command. Distinguish mismatches from indels/ambiguous bases.
- [x] Implement preparation: query VCF records across relevant reference
  chromosomes; exclude multiallelic/non-SNP records; parse exactly one AF;
  join coordinate candidates, then verify bases and unique mapping. Retain
  excluded source rows and reasons and issue one count-summary warning.
- [x] Add tests for mismatches, multiple mappings, AF=0/1, missing AF, AF=1.2,
  multi-ALT G,T, REF/ALT lengths, empty imports/results, unchanged VCF source,
  multiple preparations, source replacement, modified files, and reversed
  alignment target/query order. Example assertion:

      expect_equal(query_allele_frequency(out)$af, .25)
      expect_equal(query_allele_frequency(out)$qstart, 901)

- [x] Run the focused data tests and relevant Syn object tests. Self-review
  source ownership, normalization, warning counts, and cache invalidation.
- [x] Commit the task files and record the tests and any limitations in the
  task report. Do not regenerate namespace/Rd files while Task 2 is editing.

## Task 2: Point geom, routing, documentation and visual verification

Files: create R/geom-allele-frequency.R,
tests/testthat/test-geom-allele-frequency.R, and a reproducible example under
inst/examples. Modify narrow link-metadata handling in R/plot-build.R and
R/syn-layer.R only as needed, plus narrow raw-layout integration in R/layout.R
and R/facet-genomics.R; update DESCRIPTION, NAMESPACE, generated man
files, vignettes/ggexon-workflow.Rmd and NEWS.md.

Consumes Task 1 APIs and prepared columns. Produces geom_allele_frequency()
with normal point aesthetics, explicit table support, and implicit SynSpecies
preparation selection. It never performs VCF/MUMmer file parsing in drawing.

- [x] Write failing end-to-end plotting tests using source fixtures and tiny
  annotation tracks. AF=0/.25/.5/1 must give the literal corresponding fraction
  between independently transformed endpoints. A reversed stored alignment
  must preserve AF=0 at the reference and AF=1 at the non-reference edge.
- [x] Run devtools::test(filter="geom-allele-frequency") and observe failure.
- [x] Inherit GeomPoint, preserve link metadata, and reuse source-panel
  transforms. One compatible internal representation is the existing four
  zero-width link vertices at tstart=tend and qstart=qend; attach AF and
  reference_is_target to each group after GeomNucLink setup. That representation
  already follows genomic compression. Interpolate the transformed t and q
  endpoints and delegate rendering to GeomPoint using an identity transform
  for the already normalized coordinates. Reject flipped/non-Cartesian plots.
- [x] Add syn_data dispatch selecting a unique named preparation, routing to
  the stored alignment track and respecting selected chromosome/windows.
  Narrowly generalize link-metadata preservation by semantic link role instead
  of hard-coding GeomNucLink. Missing ribbon/source panels must be explicit.
- [x] Test genomic intron compression, panel order, standard colour/shape/size
  mappings, empty matches, multiple preparations, explicit data, and invalid
  frequencies. Inspect the actual points grob coordinates, not only build
  tables or helper results.
- [x] Generate docs with devtools::document(); ensure new files are included
  in Collate and the APIs are exported. Add an executable self-contained tiny
  VCF/MUMmer example, source ownership details, supported show-snps command,
  interpretation, and exclusion inspection to documentation.
- [x] Run focused plotting/data tests and the full test suite. Render the
  example to PNG and PDF in a temporary verification directory and visually
  inspect the result. Run package check without rebuilding the website.
- [x] Request independent code review against the approved spec. Fix important
  findings, rerun affected checks, and commit the verified feature. Keep the
  feature branch and worktree available for user review; do not merge/push.


## Completion record

Implemented on codex/allele-frequency. Runtime implementation and all final
review fixes are committed through 9af56ee1792b4995ce1f028c466bcfe6de24acc8.
Independent task reviews and the final whole-feature review approved the code.

- Full source suite: 5,244 passing assertions, zero failures, warnings or skips.
- Fresh installed focused suite: 410 passing assertions, zero failures,
  warnings or skips, including the three final regressions.
- Final same-tarball R CMD check: exit 0, zero errors, zero warnings, three
  notes concerning the linked-worktree Git pointer, installed package size,
  and dependency usage. Tests were disabled in this last check because the
  full source suite and fresh installed focused suite ran separately.
- Repository index requests used an 8-second per-command timeout; check
  completed against locally installed dependencies. No persistent R
  configuration or dependency installations were changed.
- Native example and explicit-table overlay were rendered and inspected.
  PNG and vector PDF previews are available with the task artifacts.

Review fixes also covered stale legacy VCF region caches, collisions with
unrelated user metadata, arbitrary raw track labels, duplicate link panels
across layers, exact alignment track-column matching, per-SNP fixed styling
through windows/repeated builds, and warning-free empty constant mappings.

The runnable package example is inst/examples/allele-frequency.R.
The implementation is retained on its feature branch for user review.
