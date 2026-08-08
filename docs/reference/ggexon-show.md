# Display ggexon S4 objects

Compact [`show()`](https://rdrr.io/r/methods/show.html) methods that
print a one-screen summary of the major `ggexon` S4 objects
(`SynSpecies`, `SynIndividual`, `SynLayout`, `SynLocusSet`,
`SynAnnotation`, and `HomologyAnnotation`).

## Usage

``` r
# S4 method for class 'SynAnnotation'
show(object)

# S4 method for class 'SynIndividual'
show(object)

# S4 method for class 'SynLocusSet'
show(object)

# S4 method for class 'HomologyAnnotation'
show(object)

# S4 method for class 'SynSpecies'
show(object)

# S4 method for class 'SynLayout'
show(object)
```

## Arguments

- object:

  A `ggexon` S4 object.

## Value

`object`, invisibly; called for the side effect of printing a summary.
