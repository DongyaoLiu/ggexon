af_data_fixture <- function(snp_lines = NULL, vcf_rows = NULL,
                            reverse_alignment = FALSE, track = NULL) {
  snp_file <- withr::local_tempfile(.local_envir = parent.frame())
  vcf_file <- withr::local_tempfile(fileext = ".vcf", .local_envir = parent.frame())
  if (is.null(snp_lines)) {
    snp_lines <- "101\tA\tG\t901\t101\t101\t2000\t2000\t1\t1\tchrR\tchrQ"
  }
  if (is.null(vcf_rows)) {
    vcf_rows <- "chrR\t101\trs1\tA\tG\t60\tPASS\tAF=0.25"
  }
  writeLines(snp_lines, snp_file)
  writeLines(c(
    "##fileformat=VCFv4.2",
    '##INFO=<ID=AF,Number=A,Type=Float,Description="Allele frequency">',
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
    vcf_rows
  ), vcf_file)

  ref <- SynIndividual(id = "ref")
  query <- SynIndividual(id = "query")
  vcf <- SynVCFAnnotation("population", vcf_file, metadata = list(cohort = "test"))
  ref <- add_annotation(ref, vcf)
  x <- SynSpecies("strains") |>
    add_individual(ref) |>
    add_individual(query)
  pair_data <- data.frame(
    qchr = if (reverse_alignment) "chrR" else "chrQ",
    qstart = 1L, qend = 2000L, tchr = if (reverse_alignment) "chrQ" else "chrR",
    tstart = 1L, tend = 2000L, strand = "+", alen = 2000L
  )
  if (!is.null(track)) {
    pair_data$track <- track
  }
  pair <- SynPairAlignment(
    "pair", if (reverse_alignment) "ref" else "query",
    if (reverse_alignment) "query" else "ref", "pair.paf", data = pair_data
  )
  x <- add_pairwise_alignment(x, pair)
  list(x = x, snp_file = snp_file, vcf_file = vcf_file, vcf = vcf, pair = pair)
}

af_data_import <- function(fixture) {
  SynSNPAnnotation("snps", fixture$snp_file, "ref", "query", "pair")
}

af_data_prepare <- function(fixture, name = "allele_frequency", af_field = "AF") {
  add_snp_annotation(fixture$x, af_data_import(fixture)) |>
    prepare_allele_frequency("population", "snps", name, af_field)
}
