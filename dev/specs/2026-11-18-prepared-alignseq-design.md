# Prepared nucleotide sequence detail on Syn objects

Status: approved by the user on 2026-11-18; the human connector targets the upper d2d3 row.

## Requested result

1. Replace the AS3MT tutorial's explicit DNA layer tables and
   `as3mt_plot_data()` callbacks with package preparation, inspection, and
   context-aware dispatch.
2. In the combined gene-model figure, hide the red deletion triangle and its
   event-marker label, and connect nucleotide callouts directly to the chosen
   transcript model. Preserve aligned bases, gaps, mismatch/indel colours,
   genomic coordinates, and the deletion evidence described in the caption.
3. Place human d2d3 above human full-length AS3MT. Keep exon labels attached to
   their own transcript rows throughout the tutorial.
4. Preserve the preceding local edit that removed repeat rectangles and donor
   overlay segments from the combined figure.

## Recommended architecture

Use named prepared entries in participating `SynIndividual` plot caches,
following `prepare_gene_lollipop()`. The existing Syn layer dispatcher selects
the appropriate individuals, annotation panels, chromosomes, and windows.
Prepared data carry native genomic coordinates and sequence identities, never
facet PANEL or scale IDs.

Keep `alignseq_data()` as the existing low-level, pure table constructor.
`prepare_alignseq()` calls it, stores its result and preparation recipe on the
Syn object, and returns the updated object. Original aligned strings, reference
identity, anchors, and declared boundaries remain inspectable.

This avoids adding a biological annotation class solely for derived display
data. Keeping only an AS3MT-specific metadata reader would retain the manual
dispatch that prompted this change.

## Proposed public interface

- `prepare_alignseq(x, aligned, anchors, reference, name = "alignseq", ...)`:
  accept a Syn object, validated aligned DNA strings and native anchors;
  support the existing region/detail/flank selectors and optional verified
  boundary records. Anchor tracks must identify individuals in the object.
  No alignment is inferred from equal-length strings or from ungapped PAF
  intervals.
- `query_alignseq(x, name = NULL, ...)`: inspect stored sequence/event rows,
  with individual and genomic selectors; do not change the object.
- `geom_alignseq(data = NULL, alignment = NULL, ...)`: with Syn input, resolve
  the named prepared entry through `LayerSyn`; automatically select the only
  applicable entry and require a name when several apply. Continue to accept
  existing explicit tables unchanged.
- Add `show_markers`, default TRUE, to control event glyphs and their labels
  without changing the underlying sequence evidence or difference colours.
- Add a transcript connector mode with an explicit transcript selection per
  individual. Resolve connector attachment from the corresponding displayed
  gene-model row and geometry during plot building, rather than maintaining a
  separate example-specific y position. Missing or ambiguous target geometry
  must produce an actionable error.

The final figure should read like:

```r
ggexon(as3mt) +
  geom_exon2(...) +
  geom_alignseq(
    alignment = "as3mt_vntr", show_markers = FALSE,
    anchor = "transcript", transcripts = c(human = "human_short", chimp = "chimp"),
    colour_scheme = "difference"
  )
```

The displayed example assumes the upper human d2d3 row is the selected target;
the user may instead choose full-length AS3MT or both human transcript rows.

## Tutorial migration

Prepare DNA views with the package verb and inspect them with its query. Remove
DNA preparation and DNA table retrieval from the example-specific plot helper.
Keep example-specific provenance loading and exon text preparation scoped to
the example. Set the human transcript row order before plotting and derive
label positions from those rows. Use native dispatch in both the nucleotide
close-up and the combined figure.

Update accompanying text and figure descriptions to match the displayed
connector, marker, and transcript-order behaviour. Do not alter genomic source
files, repeat alignment evidence, or the published release tag.

## Validation

- Compare prepared/query data with `alignseq_data()` for matches, substitutions,
  contiguous indels, ambiguous bases, gaps, and reverse strands.
- Check object copy behaviour, named-entry ambiguity, missing preparations,
  cleared caches, invalid input, and source/target changes.
- Check Syn dispatch for selected individuals, chromosome/windows, and custom
  annotation panel names; ensure sequence data never populate link panels.
- Render explicit-table calls to preserve existing behaviour.
- Inspect rendered grobs to verify that marker suppression removes event
  triangles while retaining sequence blocks and their connectors.
- Verify connector start coordinates against the chosen displayed transcript
  geometry, including after row reordering and genomic compression.
- Render the complete AS3MT tutorial and inspect its final figure, verifying
  d2d3 above full length and correct exon labels.

## Approved connector target

Connect once to the upper human d2d3 row and once to the chimpanzee transcript.
The human full-length model remains below d2d3.
