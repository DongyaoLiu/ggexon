# Edit homology rows

These S4 methods add, delete, or replace rows in a `HomologyAnnotation`
table. Methods for `SynSpecies` edit one attached homology annotation
selected by `name`, by `query_species`, or by omission when exactly one
homology annotation is attached.

## Usage

``` r
add_homology(x, ...)

delete_homology(x, ...)

replace_homology(x, ...)

# S4 method for class 'HomologyAnnotation'
add_homology(
  x,
  data = NULL,
  query_gene = NULL,
  reference_gene = NULL,
  ...,
  overwrite = FALSE
)

# S4 method for class 'HomologyAnnotation'
replace_homology(
  x,
  data = NULL,
  query_gene = NULL,
  reference_gene = NULL,
  ...,
  add_missing = FALSE
)

# S4 method for class 'HomologyAnnotation'
delete_homology(
  x,
  data = NULL,
  query_gene = NULL,
  reference_gene = NULL,
  ...,
  missing = c("error", "warn", "ignore")
)

# S4 method for class 'ANY'
add_homology(x, ...)

# S4 method for class 'ANY'
replace_homology(x, ...)

# S4 method for class 'ANY'
delete_homology(x, ...)

# S4 method for class 'SynSpecies'
add_homology(
  x,
  data = NULL,
  query_gene = NULL,
  reference_gene = NULL,
  ...,
  name = NULL,
  query_species = NULL,
  overwrite = FALSE
)

# S4 method for class 'SynSpecies'
replace_homology(
  x,
  data = NULL,
  query_gene = NULL,
  reference_gene = NULL,
  ...,
  name = NULL,
  query_species = NULL,
  add_missing = FALSE
)

# S4 method for class 'SynSpecies'
delete_homology(
  x,
  data = NULL,
  query_gene = NULL,
  reference_gene = NULL,
  ...,
  name = NULL,
  query_species = NULL,
  missing = c("error", "warn", "ignore")
)
```

## Arguments

- x:

  A `HomologyAnnotation` or `SynSpecies` object.

- ...:

  Extra homology table columns for `add_homology()` and
  `replace_homology()`.

- data:

  Optional data frame of rows. For `add_homology()` it must contain
  `query_gene` and `reference_gene`. For `replace_homology()` it must
  contain `query_gene` plus at least one column to update. For
  `delete_homology()`, only `query_gene` and optional `reference_gene`
  are used.

- query_gene:

  Query-side gene IDs.

- reference_gene:

  Reference-side gene IDs. In `delete_homology()` this is an optional
  guard: rows are deleted only when the current reference gene matches.

- overwrite:

  For `add_homology()`, whether incoming rows for existing `query_gene`
  values should update those rows. When `FALSE`, existing rows are kept
  and a warning is emitted.

- add_missing:

  For `replace_homology()`, whether missing `query_gene` values should
  be added. Missing rows require non-empty `reference_gene` values.

- missing:

  For `delete_homology()`, behavior when a requested `query_gene` is
  absent.

- name:

  Optional homology annotation name when `x` is a `SynSpecies`.

- query_species:

  Optional query species selector when `x` is a `SynSpecies`.

## Value

The updated object.
