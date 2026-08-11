# Load alignment data into Syn-aware alignment objects

Parses supported alignment files and caches the parsed data on alignment
objects. Pairwise alignments can currently be loaded from PAF, PSL, and
ODGI sources, and multiple alignments can be loaded when
`format = "odgi"` points to an ODGI node-table TSV or raw `.og` graph
file. When `x` is a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md),
every stored pairwise and multiple alignment is loaded and the updated
`SynSpecies` object is returned.

## Usage

``` r
load_alignment(
  x,
  odgi = NULL,
  python = NULL,
  more = NULL,
  cigar = NULL,
  alignment = NULL
)

# S4 method for class 'SynPairAlignment'
load_alignment(
  x,
  odgi = NULL,
  python = NULL,
  more = NULL,
  cigar = NULL,
  alignment = NULL
)

# S4 method for class 'SynMultiAlignment'
load_alignment(
  x,
  odgi = NULL,
  python = NULL,
  more = NULL,
  cigar = NULL,
  alignment = NULL
)

# S4 method for class 'SynSpecies'
load_alignment(
  x,
  odgi = NULL,
  python = NULL,
  more = NULL,
  cigar = NULL,
  alignment = NULL
)
```

## Arguments

- x:

  A
  [`SynPairAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynPairAlignment.md),
  [`SynMultiAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynMultiAlignment.md),
  or
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  object.

- odgi:

  Optional path to the `odgi` executable. Used when loading ODGI
  multiple alignments from raw `.og` graph files.

- python:

  Optional path to the Python interpreter. Used when loading ODGI
  multiple alignments from raw `.og` graph files.

- more:

  Logical or `NULL`; when `TRUE` and `x` is a PSL-backed
  [`SynPairAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynPairAlignment.md),
  expand each PSL record into one row per ungapped block before caching
  the parsed data. When `NULL`, preserve any existing cached PSL detail
  level and default unloaded PSL alignments to the coarse
  one-row-per-record representation.

- cigar:

  Logical or `NULL`; when `TRUE` and `x` is a PAF-backed
  [`SynPairAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynPairAlignment.md),
  expand each `cg:Z:` CIGAR string into one row per match block before
  caching the parsed data. Only match operations are emitted; gap and
  mismatch operations are used only to advance coordinates. When `NULL`,
  preserve any existing cached PAF detail level and default unloaded PAF
  alignments to the coarse one-row-per-record representation.

- alignment:

  Optional stored alignment name when `x` is a `SynSpecies`. When
  omitted, all stored pairwise and multiple alignments are loaded.

## Value

An updated object of the same class as `x`.

## Details

Unloaded `SynMultiAlignment` objects with `format = "maf"` are not yet
supported because the package does not currently provide a MAF parser.
For PSL-backed
[`SynPairAlignment`](https://dongyaoliu.github.io/ggexon/reference/SynPairAlignment.md)
objects, repeated calls to `load_alignment()` with different explicit
`more` values will replace the cached data so the in-memory
representation matches the requested detail level, while `more = NULL`
preserves any existing cached PSL mode.

This is an S4 generic that dispatches on the class of `x`.

## Examples

``` r
paf_path <- system.file("extdata", "V_alginment.paf", package = "ggexon")
pair <- SynPairAlignment(
  name = "XZ1516_vs_N2",
  query_individual = "XZ1516",
  target_individual = "N2",
  file = paf_path
)
#> Error in validObject(.Object): invalid class “SynPairAlignment” object: `source_file` must be a non-empty character vector with no empty entries.
pair <- load_alignment(pair)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'load_alignment': object 'pair' not found
```
