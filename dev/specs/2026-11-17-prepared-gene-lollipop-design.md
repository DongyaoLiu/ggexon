# Prepared gene-lollipop grids on Syn objects

## Purpose

Replace the MHC tutorial's hand-built layer data and metadata callbacks with a
package preparation verb. A Syn object should carry the selected gene data and
a shared gene-slot grid. `geom_gene_lollipop()` should discover that preparation
through the normal Syn plot context.

This document records the behavior, public interface and drawing contract
approved during the design conversation. Implementation and verification are
tracked in the accompanying implementation plan.

## Agreed behavior

- Automatically assign genes to shared slots, with optional manual overrides.
- Accept an explicit feature-metadata key or stored `HomologyAnnotation`
  mappings to establish shared gene identities.
- Support both row and column counts. Infer the missing dimension when only
  one is supplied.
- Use the same row-and-column slot for a shared identity in every panel. Keep
  empty cells when a panel has no matching selected gene.
- Order shared genes using a selected reference individual. Use Human CHM13
  (`HSAh1`) for the MHC tutorial.
- Place genes missing from the reference near their shared flanking genes.
  Require an order override if placement is ambiguous or contradictory.
- Retain all copies of duplicated genes in separate adjacent slots. Optional
  copy keys can establish correspondence. Automatic copy ordering is a display
  convention, not an assertion of one-to-one copy orthology.
- Automatically use the only applicable preparation. Require its name when
  several applicable preparations exist.
- Keep biological coordinates native. Compute physical head geometry and
  leader paths on the output device.

## Architecture

Use named prepared entries in each participating `SynIndividual`'s existing
`plot_cache`, following `prepare_genomic_layout()`. Keep a small name/source
registry under package-owned metadata so existing cache-clearing setters can
be recognized as invalidating a preparation. Computed dataframes remain in
`plot_cache`. A preparation on a
`SynSpecies` computes the shared grid once and attaches the same grid identity
and recipe, with each individual's selected gene rows, to its individuals.
Preparing a `SynIndividual` produces a one-individual grid.

The existing `LayerSyn` and geom `syn_data` route remains the data dispatcher.
The dispatcher reads a prepared entry, binds it to the actual context tracks,
and applies the context's chromosome and genomic-window selection. Preparation
does not persist facet `PANEL`, `ROW`, `COL`, or scale identifiers.

This is preferable to a new biological annotation class because these are
derived plotting data. A new annotation class would add import, annotation
selection, and class-maintenance work without changing the source gene models.
Keeping only the current example helper would be the smallest change, but
would retain the manual dataframe construction the user wants to remove.

## Proposed public interface

### Preparation

`prepare_gene_lollipop()` accepts a `SynIndividual` or `SynSpecies` and returns
the updated object. Its interface covers:

- `name`: the prepared entry's name, defaulting to `"gene_lollipop"`.
- `individuals`: optional participating individual IDs; otherwise use all
  individuals in the object.
- `annotation`: optional source feature-annotation name; otherwise record each
  individual's active feature annotation at preparation time.
- `genes`: optional gene IDs/names, using the existing gene-query semantics;
  a named list supports different selectors per individual.
- `where`: an optional named list of feature-metadata values. Values within a
  field are alternatives; different fields are combined with AND. For MHC,
  `where = list(gene_role = "MHC")` selects the curated genes without building
  a layer dataframe.
- `key`: optional metadata-column name for a curated shared identity.
- `homology`: optional stored homology-annotation names. Without `key`, use
  compatible stored mappings to the chosen reference. Multiple conflicting
  mappings require an explicit choice. Supplying both an explicit `key` and
  explicit `homology` is an error.
- `copy_key`: optional metadata-column name identifying corresponding copies.
- `reference`: reference individual ID. Infer itself for a single individual,
  or a unique common homology reference; otherwise require it explicitly.
- `nrow`, `ncol`: positive integer grid dimensions. Require at least one;
  infer a missing dimension from the number of slots, including extra copies.
- `order`: optional complete ordering of shared gene keys to resolve or
  override reference/flank ordering.
- `slots`: optional partial table of explicit key/copy, row, and column
  assignments. Validate it against the grid and assign remaining slots
  automatically in the remaining cells.
- `label`, `fill`: optional metadata-column names. Defaults use nonempty
  `plot_label` and `plot_fill` where present, with the existing gene-label and
  fill fallbacks.

An explicit metadata key takes precedence over automatically discovered
homology. Repeated display labels alone do not establish a shared identity.
Unmatched genes receive individual-qualified identities and remain visible.
Only actual homology hits establish shared identities; the dispatcher's
display-name fallback is not homology evidence.

### Inspection

`query_gene_lollipop()` accepts the object, an optional preparation name, and
a `component` selecting `"genes"` or `"slots"`. Infer the name only when it is
unambiguous. Return a dataframe after validating the preparation.

Gene rows retain individual ID, source annotation, stable feature ID, source
gene name, chromosome, native bounds, midpoint anchor, native strand, label,
fill, shared key, copy identity, and assigned slot row/column. Preserve other
non-conflicting source metadata for aesthetic mappings.

The slot table includes every shared slot, its ordering and row/column,
whether its order/copy identity was automatic or explicit, and occupancy by
individual. It makes empty cells and automatic copy correspondence inspectable.
An empty cell means no selected annotated gene occupies that cell; it does not
by itself establish biological absence.

### Plotting

Add `lollipop_layout` to `geom_gene_lollipop()` as the explicit prepared-name
selector. With implicit Syn data:

1. An explicit name selects that preparation or gives a clear error.
2. Exactly one applicable preparation is selected automatically.
3. Multiple applicable names require an explicit selection.
4. With no current or registered preparation, retain the existing native-gene
   midpoint behavior.

A registered preparation whose cache was cleared is invalidated, not absent.
Report that it must be prepared again rather than silently switching to native
midpoint mode.

Explicit dataframe input retains its current behavior. A named prepared
selector with dataframe input is an error rather than an ignored argument.

Prepared input supplies anchors, strand, labels, and slot identities. Aesthetic
styling remains available. Remapping prepared coordinates or row/column
identities is rejected with an instruction to change the preparation. Existing
compact and automatic-column packing remain available for unprepared input.

## Ordering and duplicate rules

Reference genes are ordered by native genomic midpoint, with stable feature ID
as a tie breaker. A complete explicit `order` takes precedence. Shared keys
missing from the reference use constraints from their nearest shared flanking
genes. Resolve only a unique insertion order; list unresolved keys and flanks
in an error when an override is needed. Rearrangements of already ordered
reference keys do not change the reference ordering.

An explicit copy key is used as supplied and must be complete and unique
within each individual/shared-key group. Without it, keep the maximum observed
copy count for each shared key and assign deterministic local genomic copy
order. Corresponding ordinal copy cells are explicitly marked as automatic
display correspondence. Never merge copies or discard a lower-scoring copy.

Assign slots in reference-key order, top to bottom within a column, then left
to right. Keep duplicate copy slots consecutive in this reading order. Partial
slot overrides must not collide or contradict the requested ordering.

## Proposed physical grid contract

The prepared mode draws a regular shared grid: equal column centres across
equally sized annotation panels, common row pitch, and a common head size
determined from the preparation's labels. Unoccupied cells remain reserved.
Native genomic positions determine the anchors and leader origins, while the
slot grid determines head positions. This distinction should be explicit in
the tutorial and figure caption.

Plot-window filtering removes out-of-window anchors and their heads without
repacking the remaining slots. Reversing a source panel changes the native
anchor display and arrow direction exactly once; it does not reorder the
shared reference grid. Leader crossings caused by different native gene orders
are allowed; the grid must not conceal rearrangements by reordering a panel.

Text measurement, millimetre spacing, gutter lanes, and final head/leader
coordinates remain draw-time operations. Existing head-width, head-height,
headroom, and maximum-shift constraints remain enforced. Insufficient space
gives an actionable error; do not drop genes, shrink text, or silently change
the grid. A shared regular grid can need a larger explicit `max_shift_mm` than
the current local packing mode.

A 4-by-6 grid has numerical capacity for the MHC example's 23 keys, but that
does not guarantee physical fit or short leaders. Choose final MHC dimensions
and displacement allowance from a rendered figure.

## Stored state and freshness

Each entry stores selected native gene rows, the shared slot table, the
preparation recipe, source annotation names, a common grid identity, and source
fingerprints. It stores no device-specific millimetre positions or plot-panel
IDs. The recipe records the identity source, reference order, copy convention,
grid dimensions, and overrides.

Validate the selected source features and relevant metadata before querying
or dispatching. When using a species object, also validate the homology inputs
and compatibility of participating entries. Changed source data or incompatible
grid identities require preparation again. Never silently compute a new grid
during a plot build. A prepared individual extracted from a species retains
the stored shared grid and validates its own available sources.

## Tutorial migration and verification

Update Tutorial 04 to prepare the MHC grid directly on the Syn object and plot
it through native dispatch. Remove the example-specific lollipop dataframe
builder and callback. Keep the selected Siamang H1, Gorilla H1, Human CHM13,
all 58 functional MHC genes, and the 443 retained non-pseudogene context genes.
Keep the native gene-span layer and pairwise alignments. The new verb does not
perform biological pseudogene classification.

The MHC source keys absent from the selected human reference are H, OKO, J,
and DRB3. Some flanking constraints can leave several valid insertions; document
an explicit ordering override where required rather than inventing certainty.

Verify selection and metadata retention, homology identity versus fallback,
reference ordering and ambiguous flanks, duplicate retention, inferred grid
dimensions, explicit slots and collisions, stale sources, and automatic versus
named dispatch. Integration checks cover real Syn contexts, cropped/reversed
panels, unchanged slot identities, native anchors, empty cells, and existing
unprepared/dataframe behavior. Render PDF and PNG at the tutorial size and an
additional size; inspect all 58 heads, shared grid alignment, native anchor
positions, label fit, and leader routing. Rebuild the tutorial and website.

Implementation touches a new preparation module, the existing geom and layout
helpers, tests, Tutorial 04 and its render/helper code, plus generated exports,
reference pages and website files: more than eight files in total. Preserve all
unrelated uncommitted work. Do not commit or publish as part of this change.
