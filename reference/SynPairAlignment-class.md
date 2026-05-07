# SynPairAlignment class

`SynPairAlignment` stores one pairwise alignment between two
`SynIndividual` objects in a `SynSpecies` collection. As a concrete
`SynSpeAnnotation`, the object keeps the shared annotation metadata
together with the query/target identifiers used to route link panels and
optional cached parsed alignment data. For PSL-backed alignments, the
cached table can be stored either at the original one-row-per-record
level or at a detailed one-row-per-ungapped-block level when loaded with
`load_alignment(more = TRUE)`.

## Slots

- `name`:

  Unique alignment label used to retrieve the object from a
  `SynSpecies`.

- `query_individual`:

  Query-side `SynIndividual` identifier.

- `target_individual`:

  Target-side `SynIndividual` identifier.

- `source_file`:

  Path to the alignment file on disk.

- `format`:

  Alignment file format. Currently `"paf"`, `"psl"`, or `"odgi"`.

- `data`:

  Optional cached parsed alignment data. For PSL files this can be
  either one row per PSL record or one row per ungapped block, depending
  on how the object was loaded.

- `metadata`:

  Optional user or import metadata. Loader state such as the cached PSL
  detail mode may also be stored here.

## Prototype defaults

- `annotation_scope = "species"`

- `lazy = TRUE`

- `loaded = FALSE`

- `format = "paf"`

- `data = NULL`

## Validity rules

- `query_individual` and `target_individual` must each be one non-empty
  character value.

- `query_individual` and `target_individual` must differ.

- `format` must currently be `"paf"`, `"psl"`, or `"odgi"`.

## Cached PSL detail modes

- `load_alignment(more = FALSE)` keeps one cached row per PSL record.

- `load_alignment(more = TRUE)` expands each PSL record into one cached
  row per ungapped block and records that detail level in
  `metadata$psl_more`.
