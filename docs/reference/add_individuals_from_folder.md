# Add many annotation files from a folder as SynIndividuals

Discovers supported annotation files in a folder and adds one
[`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
per file to a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
object. Supported extensions are `.gff`, `.gff3`, and `.gtf` (optionally
with a trailing `.gz`). When `annotation_format = "auto"`, the format is
inferred from the file extension.

## Usage

``` r
add_individuals_from_folder(
  x,
  folder,
  annotation_format = c("auto", "gff", "gtf"),
  recursive = FALSE
)

# S4 method for class 'SynSpecies'
add_individuals_from_folder(
  x,
  folder,
  annotation_format = c("auto", "gff", "gtf"),
  recursive = FALSE
)

# S4 method for class 'ANY'
add_individuals_from_folder(
  x,
  folder,
  annotation_format = c("auto", "gff", "gtf"),
  recursive = FALSE
)
```

## Arguments

- x:

  A
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  object.

- folder:

  Path to a directory containing annotation files.

- annotation_format:

  One of `"auto"`, `"gff"`, or `"gtf"`. When `"auto"`, files with
  supported extensions are discovered and each file's format is inferred
  from its extension. When `"gff"` or `"gtf"`, only files with matching
  extensions are imported.

- recursive:

  Logical; should files be discovered recursively?

## Value

An updated
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
object.

## Details

Individual ids default to the filename stem with the annotation
extension removed, so a file such as `N2.gff3` becomes a `SynIndividual`
with id `"N2"`.

Genome files are waived by default for this convenience import. That
makes the helper suitable for annotation-only workflows, while
genome-dependent operations can still be added later by replacing or
rebuilding the `SynIndividual` objects with FASTA paths.
