prepared_genomic_fixture <- function(id = "sample") {
  file <- tempfile(fileext = ".gtf")
  records <- data.frame(
    chr = "chr1", source = "fixture",
    type = c("transcript", "exon", "CDS", "exon", "CDS",
             "transcript", "exon", "CDS", "exon", "CDS"),
    start = c(100, 100, 100, 200, 200, 100, 100, 100, 300, 300),
    end = c(208, 104, 104, 208, 208, 308, 104, 104, 308, 308),
    score = ".", strand = "+", phase = c(".", ".", "0", ".", "1", ".", ".", "0", ".", "1"),
    attributes = paste0('gene_id "geneA"; transcript_id "', rep(c("tx1", "tx2"), each = 5), '";')
  )
  utils::write.table(records, file, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
  model <- load_annotation(SynIndividual(annotation_file = file, id = id))
  mutations <- data.frame(
    transcript_id = c("tx1", "tx1", "tx2"), gene_id = "geneA",
    position = 2L, mutation = c("K2R", "K2Q", "K2R"),
    ref = "K", alt = c("R", "Q", "R"), sample_count = c(1L, 10L, 3L),
    source_row_id = 1:3
  )
  model <- add_annotation(model, SynProteinMutationAnnotation(
    name = "muts", mutation_file = "<fixture>", mutation_data = mutations,
    keytype = "transcript_id", lazy = FALSE
  ))
  regions <- SynProteinDomainAnnotation("regions", "<fixture>", keytype = "transcript_id", lazy = FALSE)
  regions@domain_data <- S4Vectors::DataFrame(
    transcript_id = c("tx1", "tx2"), start = 1L, end = 3L,
    domain = "Binding", analysis = "Pfam"
  )
  regions@loaded <- TRUE
  add_annotation(model, regions)
}
