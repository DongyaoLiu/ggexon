# Resolve one individual from Syn-backed plot input

Normalizes Syn plotting inputs so downstream layer helpers can work with
a single
[`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
object. When `x` is already a `SynIndividual`, the function returns it
unchanged after optionally checking that `species` matches its
identifier. When `x` is a
[`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
collection, the helper selects one stored individual by name.

## Usage

``` r
resolve_syn_individual(x, species = NULL)
```

## Arguments

- x:

  A
  [`SynSpecies`](https://dongyaoliu.github.io/ggexon/reference/SynSpecies.md)
  or
  [`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
  object.

- species:

  Optional individual identifier. When `x` is a `SynSpecies` with more
  than one stored individual, this argument is required.

## Value

A single
[`SynIndividual`](https://dongyaoliu.github.io/ggexon/reference/SynIndividual.md)
object.

## Details

This function is mainly used inside Syn-aware geoms and query helpers
that allow users to supply either a whole `SynSpecies` object or an
already selected `SynIndividual`.

The helper throws an error when:

- `x` is neither a `SynSpecies` nor a `SynIndividual`

- the supplied `SynSpecies` has no individuals

- `species` is omitted for a `SynSpecies` that stores multiple
  individuals

- `species` does not match any stored individual

- `species` is supplied for a `SynIndividual` but does not match

## Examples

``` r
ann_path <- system.file(
  "extdata",
  "gff",
  "caenorhabditis_XZ1516.gff3",
  package = "ggexon"
)

ind <- SynIndividual(id = "XZ1516", annotation = ann_path)
#> Error in SynIndividual(id = "XZ1516", annotation = ann_path): argument 2 matches multiple formal arguments
resolve_syn_individual(ind)
#> Error in resolve_syn_individual(ind): could not find function "resolve_syn_individual"
resolve_syn_individual(ind, species = "XZ1516")
#> Error in resolve_syn_individual(ind, species = "XZ1516"): could not find function "resolve_syn_individual"

sp <- SynSpecies(name = "worms")
sp <- add_individual(sp, ind)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'individual' in selecting a method for function 'add_individual': object 'ind' not found
resolve_syn_individual(sp, species = "XZ1516")
#> Error in resolve_syn_individual(sp, species = "XZ1516"): could not find function "resolve_syn_individual"
```
