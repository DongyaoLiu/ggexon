# Reset recorded annotation patches

`reset_patches()` restores the base feature annotation, removes recorded
patches, reapplies display labels, and clears derived feature indexes,
sequence caches, and feature-layer plotting state. `clear_patches()`
remains a compatible synonym with the same arguments and behavior.

## Usage

``` r
reset_patches(x, annotation = NULL)

clear_patches(x, annotation = NULL)
```

## Arguments

- x:

  A `SynFeatureAnnotation` or `SynIndividual` object.

- annotation:

  Optional feature-annotation layer name when `x` is a `SynIndividual`.

## Value

The updated object.

## See also

[`patch_annotation()`](https://dongyaoliu.github.io/ggexon/reference/patch_annotation.md),
[`list_patches()`](https://dongyaoliu.github.io/ggexon/reference/list_patches.md)
