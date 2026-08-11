# Subset one or more individuals in a `SynSpecies` by coordinate string

Returns a new
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
in which selected individuals have their feature annotation layers
trimmed according to coordinate strings such as
`"XZ1516#V_RagTag:21559983-21620009"`. Individuals not listed in
`coords` are left unchanged. Any stored
[`SynLayout`](https://dongyaoliu.github.io/ggexon/reference/SynLayout.md)
is cleared because the panel metadata may no longer match the new
subsetted windows.

## Usage

``` r
subset_species(x, coords, annotations = c("all_feature", "active"))

# S4 method for class 'SynSpecies'
subset_species(x, coords, annotations = c("all_feature", "active"))
```

## Arguments

- x:

  A `SynSpecies` object.

- coords:

  One or more coordinate strings in the form
  `"species#seqname:start-end"`. This can be a single string, a
  character vector, or a list of strings.

- annotations:

  One of `"all_feature"` or `"active"`. Passed through to
  [`subset_individual()`](https://dongyaoliu.github.io/ggexon/reference/subset_individual.md).

## Value

A `SynSpecies` object.

## Details

This is an S4 generic that dispatches on the class of `x`.

## Examples

``` r
ann_path <- system.file(
  "extdata",
  "compact_synspecies",
  "caenorhabditis_XZ1516.gff3",
  package = "ggexon"
)
ind <- SynIndividual(
  annotation_file = ann_path,
  genome_file = genome_waiver(),
  id = "XZ1516"
) |>
  load_annotation()
gr <- annotation_data(ind)
coords <- paste0(
  "XZ1516#",
  as.character(GenomeInfoDb::seqnames(gr))[[1L]],
  ":",
  IRanges::start(gr)[[1L]],
  "-",
  IRanges::end(gr)[[1L]]
)

sp <- SynSpecies(name = "worms") |> add_individual(ind)
sp_window <- subset_species(sp, coords = coords)
#> subset_feature_annotation() kept 8 rows, 1 genes, and 1 transcripts.
```
