# Add or replace an annotation layer on a SynIndividual

Add or replace an annotation layer on a SynIndividual

## Usage

``` r
add_annotation(x, annotation, set_active = FALSE)

# S4 method for class 'SynIndividual,SynAnnotation'
add_annotation(x, annotation, set_active = FALSE)

# S4 method for class 'SynIndividual,ANY'
add_annotation(x, annotation, set_active = FALSE)

# S4 method for class 'ANY,SynAnnotation'
add_annotation(x, annotation, set_active = FALSE)

# S4 method for class 'ANY,ANY'
add_annotation(x, annotation, set_active = FALSE)
```

## Arguments

- x:

  A `SynIndividual` object.

- annotation:

  A `SynAnnotation` object.

- set_active:

  Logical; when `TRUE`, make this the active annotation.

## Value

An updated `SynIndividual` object.
