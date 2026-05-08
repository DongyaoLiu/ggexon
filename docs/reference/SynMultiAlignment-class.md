# SynMultiAlignment class

`SynMultiAlignment` stores one multiple alignment spanning more than two
individuals. Like `SynPairAlignment`, it is a concrete
`SynSpeAnnotation` with a source file, optional cached parsed
representation, and metadata, but it records a vector of participating
individual identifiers instead of query/target sides.

## Slots

- `name`:

  Unique alignment label used to retrieve the object from a
  `SynSpecies`.

- `individuals`:

  Character vector of included `SynIndividual` identifiers.

- `source_file`:

  Path to the alignment file on disk.

- `format`:

  Alignment file format. Currently `"maf"` or `"odgi"`.

- `data`:

  Optional cached parsed alignment data.

- `metadata`:

  Optional user or import metadata.

## Prototype defaults

- `annotation_scope = "species"`

- `lazy = TRUE`

- `loaded = FALSE`

- `individuals = character()`

- `format = "maf"`

- `data = NULL`

## Validity rules

- `individuals` must contain at least two non-empty character values.

- `individuals` must not contain duplicates.

- `format` must currently be `"maf"` or `"odgi"`.
