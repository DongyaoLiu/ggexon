# SynAnnotation class hierarchy

`SynAnnotation` is the abstract base class for annotation layers
attached to `SynIndividual` or `SynSpecies` objects. Individual-level
and species-level annotations are represented by dedicated abstract
subclasses, with genome-coordinate and protein-coordinate annotations
further specializing the individual-level branch.

## Class hierarchy

- `SynAnnotation`: abstract base class

- `SynIndAnnotation`: abstract individual-level annotation

- `SynSpeAnnotation`: abstract species-level annotation

- `SynGenomeAnnotation`: abstract genome-coordinate individual
  annotation

- `SynProteinAnnotation`: abstract protein-coordinate individual
  annotation

- `SynFeatureAnnotation`: GFF/GTF structural annotation

- `SynAnnotationPatch`: gene-model patch record

- `SynVCFAnnotation`: VCF/BCF variant annotation

- `SynBigWigAnnotation`: BigWig signal annotation

- `SynProteinDomainAnnotation`: protein-domain annotation

- `SynProteinMutationAnnotation`: protein-coordinate mutation annotation
