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
  "caenorhabditis_XZ1516.gff3",
  package = "ggexon"
)
ind <- SynIndividual(
  annotation_file = ann_path,
  genome_file = genome_waiver(),
  id = "XZ1516"
) |>
  load_annotation()
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'load_annotation': `annotation_file` must not contain missing or empty paths.
gr <- annotation_data(ind)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'annotation_data': object 'ind' not found
coords <- paste0(
  "XZ1516#",
  as.character(GenomeInfoDb::seqnames(gr))[[1L]],
  ":",
  IRanges::start(gr)[[1L]],
  "-",
  IRanges::end(gr)[[1L]]
)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'seqnames': object 'gr' not found

sp <- SynSpecies(name = "worms") |> add_individual(ind)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'individual' in selecting a method for function 'add_individual': object 'ind' not found
sp_window <- subset_species(sp, coords = coords)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'subset_species': object 'sp' not found
```
