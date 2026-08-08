# SynBigWigAnnotation class

`SynBigWigAnnotation` stores a region-queryable signal layer backed by a
BigWig file.

## Details

In addition to the slots listed below, the class inherits `name`,
`source_file`, `annotation_scope`, `lazy`, `loaded`, `metadata`, and
`plot_cache` from `SynAnnotation`.

## Slots

- `data_format`:

  Signal file format label. Currently `"bigwig"`.

- `signal`:

  Compatibility slot for serialized cached signal data. Queries do not
  populate or update this slot.

- `seqinfo`:

  Compatibility slot for serialized `Seqinfo` data. Queries do not
  populate or update this slot.

- `window_cache`:

  Compatibility slot for serialized queried windows. Queries do not
  populate or update this slot.

## Prototype defaults

- `data_format = "bigwig"`

- `annotation_scope = "nucleotide"`

- `signal = NULL`

- `seqinfo = NULL`

- `window_cache = list()`

## Validity rules

No additional custom validity checks are defined beyond the inherited
slot classes and parent-class rules.
