test_syn_individual <- function(genome_file = genome_waiver(),
                                annotation_file,
                                id = NULL,
                                annotation_format = c("auto", "gff", "gtf"),
                                annotation_name = "features",
                                metadata = list()) {
  annotation_format <- match.arg(annotation_format)

  x <- SynIndividual(
    genome_file = genome_file,
    annotation_file = annotation_file,
    id = id,
    annotation_format = annotation_format,
    metadata = metadata
  )

  ann <- SynFeatureAnnotation(
    name = annotation_name,
    annotation_file = annotation_file,
    annotation_format = annotation_format
  )

  add_annotation(x, ann, set_active = TRUE)
}
