# ggexon Object Model

This note describes the long-term object model used by `ggexon` for synteny,
annotation, and derived sequence workflows.

## Design goals

- Keep one genome-level object per species or sample.
- Allow multiple annotation layers on the same genome.
- Keep stable machine-readable identifiers separate from plot-facing labels.
- Support large files through file-backed or lazy query methods.
- Keep structural annotation, variant tracks, signal tracks, and protein-space
  annotation in distinct classes.

## Top-level objects

### `SynSpecies`

`SynSpecies` is the species-comparison container.

It owns:

- a named list of `SynIndividual` objects
- a named list of `SynPairAlignment` objects
- a named list of `SynMultiAlignment` objects
- metadata about the comparison set

This is the object that ties together within-species annotation state and
between-species alignment state.

### `SynIndividual`

`SynIndividual` is the genome-level container.

It owns:

- `id`
- `genome_file`
- `seqinfo`
- a registry of annotation layers
- one active feature annotation
- package-level metadata and plot caches

Conceptually, one `SynIndividual` represents one line or track in a synteny
plot.

## Alignment hierarchy

### `SynPairAlignment`

Represents one pairwise alignment layer, currently intended for PAF-like data.

Owns:

- `name`
- `query_individual`
- `target_individual`
- `file`
- `format`
- `data`
- `metadata`

The explicit query/target fields matter because pairwise alignment formats are
directional.

### `SynMultiAlignment`

Represents one multiple-species alignment layer, currently intended for MAF.

Owns:

- `name`
- `individuals`
- `file`
- `format`
- `data`
- `metadata`

## Annotation hierarchy

### `SynAnnotation`

Abstract base class for all annotation layers.

Shared fields:

- `name`
- `source_file`
- `annotation_scope`
- `lazy`
- `loaded`
- `metadata`
- `plot_cache`

### `SynGenomeAnnotation`

Abstract subclass for genome-coordinate annotations.

### `SynProteinAnnotation`

Abstract subclass for protein-coordinate annotations.

### `SynFeatureAnnotation`

Structural genome annotation, typically imported from GFF or GTF.

Owns:

- `annotation_format`
- `base_annotation`
- `annotation`
- `patches`
- `feature_index`
- `label_map`
- `nucleotide_seq`
- `protein_seq`

This is the feature layer used by:

- `load_annotation()`
- `query_features()`
- `extract_cds_seq()`
- `translate_protein()`
- `set_gene_labels()`
- `patch_annotation()`

### `SynVCFAnnotation`

Variant annotation layer for VCF or BCF files.

Owns:

- `data_format`
- `variants`
- `index_file`
- `genome_build`
- `region_cache`

Primary verb:

- `query_variants()`

### `SynBigWigAnnotation`

Signal track for BigWig files.

Owns:

- `data_format`
- `signal`
- `seqinfo`
- `window_cache`

Primary verb:

- `query_signal()`

### `SynProteinDomainAnnotation`

Protein-space annotation such as InterPro or Pfam tables.

Owns:

- `data_format`
- `domain_data`
- `keytype`
- `source_db`

Primary verb:

- `query_domains()`

## Active feature annotation

`SynIndividual` may contain many annotation layers, but only a
`SynFeatureAnnotation` can serve as the active structural layer.

This avoids ambiguity when other layers such as VCF or BigWig are attached.

Primary helpers:

- `annotation_names()`
- `get_annotation()`
- `add_annotation()`
- `active_feature_annotation()`
- `set_active_feature_annotation()`

## Stable IDs and plot labels

Stable IDs from the source annotation should not be replaced.

Instead, `SynFeatureAnnotation` stores a label mapping and writes a
`plot_label` column into the annotation metadata when available.

This keeps:

- stable IDs for joins, translation, and patching
- readable labels for plotting

Primary verb:

- `set_gene_labels()`

## Patch workflow

`SynFeatureAnnotation` supports curated correction patches.

Important fields:

- `base_annotation`: original imported annotation
- `patches`: list of `SynAnnotationPatch`
- `annotation`: current materialized annotation after patch application

### `SynAnnotationPatch`

A patch record stores:

- `name`
- `patch_data`
- `target_ids`
- `mode`
- `metadata`

Supported modes:

- `replace`
- `add`
- `drop`

Primary verbs:

- `patch_annotation()`
- `patch_annotation_from_gff()`
- `read_patch_gff()`
- `list_patches()`
- `clear_patches()`

Patch application invalidates stale derived caches such as CDS and protein
translations.

## Lazy-loading strategy

### Eager enough to be useful

Usually loaded as full objects:

- `SynFeatureAnnotation` GFF/GTF structure

### File-backed or region queried

Usually kept lazy:

- `SynVCFAnnotation`
- `SynBigWigAnnotation`

### Table-backed and filtered

Usually lazily loaded or loaded once on demand:

- `SynProteinDomainAnnotation`

## Current workflow shape

Typical user workflow:

1. Create one or more `SynIndividual` objects.
2. Load the active feature annotation for each individual as needed.
3. Attach optional signal, variant, or protein-domain layers.
4. Apply label mappings.
5. Apply curated annotation patches if needed.
6. Translate proteins or extract CDS from active feature annotations.
7. Group individuals inside a `SynSpecies` object.
8. Attach pairwise or multiple alignments to describe cross-species structure.
9. Plot with `ggexon`.

For example, the intended plotting workflow is now:

```r
sp <- SynSpecies(name = "Caenorhabditis")
sp <- add_individual(sp, x)

ggexon(sp) +
  geom_exon(
    chr = "RagTag_V",
    subset = c(21550000, 21680000)
  )
```

Here `ggexon()` keeps the `SynSpecies` object intact, while syn-aware geoms
materialize the plotting table they need during the build step. When the
`SynSpecies` object contains exactly one individual, `geom_exon()` and
`geom_gene()` use it by default. If there are multiple individuals, then
`species =` is required.

For a gene-level overview of the same region:

```r
ggexon(sp) +
  geom_gene(
    chr = "RagTag_V",
    subset = c(21550000, 21680000)
  )
```

`geom_exon()` preserves exon structure, while `geom_gene()` collapses by
`gene_id` and draws one directional span per gene. Both use absolute genomic
coordinates.

## Near-term follow-up

- Make plotting geoms prefer `plot_label` when present.
- Add `SynIndividual` convenience wrappers for non-feature layer queries.
- Tighten patch target matching for parent-child feature families.
- Document recommended input schemas for protein-domain tables.
- Add file-backed verbs for pairwise and multiple alignment layers.
