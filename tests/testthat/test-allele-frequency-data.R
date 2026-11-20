test_that("MUMmer rows preserve native coordinates and normalize reference frames", {
  fixture <- af_data_fixture(c(
    "101\tA\tG\t901\t101\t101\t2000\t2000\t1\t1\tchrR\tchrQ",
    "102\tA\tG\t902\t102\t102\t2000\t2000\t1\t-1\tchrR\tchrQ",
    "103\tT\tC\t903\t103\t103\t2000\t2000\t-1\t1\tchrR\tchrQ",
    "104\tT\tC\t904\t104\t104\t2000\t2000\t-1\t-1\tchrR\tchrQ"
  ))
  annotation <- af_data_import(fixture)
  expect_s4_class(annotation, "SynSpeAnnotation")
  rows <- query_snps(annotation)
  expect_equal(rows$reference_pos, 101:104)
  expect_equal(rows$query_pos, 901:904)
  expect_identical(rows$reference_base, rep("A", 4L))
  expect_identical(rows$query_base, rep("G", 4L))
  expect_identical(rows$strand, c("+", "-", "-", "+"))

  x <- add_snp_annotation(fixture$x, annotation)
  expect_identical(snp_annotations(x), list(snps = annotation))
  rows <- query_snps(x, "snps", chr = "chrR", start = 102, end = 103)
  expect_equal(rows$query_pos, 902:903)
})

test_that("show-snps headers identify optional overlap counts without parsing path lines", {
  fixture <- af_data_fixture(c(
    "/tmp/1 reference.fa /tmp/query.fa", "NUCMER", "",
    "[P1]\t[SUB]\t[SUB]\t[P2]\t[BUFF]\t[DIST]\t[R]\t[Q]\t[LEN R]\t[LEN Q]\t[FRM]\t[TAGS]",
    "101\tA\tG\t901\t101\t101\t1\t0\t2000\t2000\t1\t1\tchrR\tchrQ"
  ))
  annotation <- af_data_import(fixture)
  expect_equal(query_snps(annotation)$reference_pos, 101)
  expect_warning(x <- af_data_prepare(fixture), "ambiguous_mapping")
  expect_identical(query_allele_frequency(x, component = "excluded")$reason, "ambiguous_mapping")
})

test_that("unsupported headerless show-snps layouts identify the accepted command", {
  fixture <- af_data_fixture("101\tA\tG\t901\t101\t101\t1\t1\tchrR\tchrQ")
  expect_error(af_data_import(fixture), "show-snps -ClrTHI")
})

test_that("unnamed imports and unrelated alignments are rejected", {
  fixture <- af_data_fixture()
  annotation <- af_data_import(fixture)
  expect_error(add_snp_annotation(SynSpecies("empty"), annotation), "pair")
  expect_error(SynSNPAnnotation("snps", fixture$snp_file, "ref", "ref", "pair"), "differ")
  expect_error(prepare_allele_frequency(fixture$x, "population", "snps"), "snps")
})

test_that("preparation preserves AF, native positions and the VCF source", {
  fixture <- af_data_fixture()
  x <- af_data_prepare(fixture)
  variants <- query_allele_frequency(x)
  expect_equal(variants$af, 0.25)
  expect_equal(variants$tstart, 101)
  expect_equal(variants$qstart, 901)
  expect_equal(variants$source_row, 1)
  expect_identical(variants$ID, "rs1")
  expect_identical(variants$INFO, "AF=0.25")
  expect_identical(variants$track, "link_pair")
  expect_identical(variants$reference_is_target, TRUE)
  expect_identical(get_annotation(individuals(x)[["ref"]], "population"), fixture$vcf)
})

test_that("stored alignment order determines columns while AF stays reference-relative", {
  fixture <- af_data_fixture(
    "101\tA\tG\t901\t101\t101\t2000\t2000\t1\t-1\tchrR\tchrQ",
    reverse_alignment = TRUE, track = "custom ribbon"
  )
  variants <- query_allele_frequency(af_data_prepare(fixture))
  expect_identical(variants$tspecies, "query")
  expect_identical(variants$qspecies, "ref")
  expect_identical(variants$tchr, "chrQ")
  expect_identical(variants$qchr, "chrR")
  expect_identical(variants$track, "custom ribbon")
  expect_identical(variants$reference_is_target, FALSE)
  expect_identical(variants$strand, "-")
  expect_equal(variants$tstart, 901)
  expect_equal(variants$qstart, 101)
  expect_equal(variants$af, 0.25)
})

test_that("invalid VCF rows are retained with one warning summarizing exclusions", {
  fixture <- af_data_fixture(c(
    "101\tA\tG\t901\t101\t101\t2000\t2000\t1\t1\tchrR\tchrQ",
    "102\tA\tG\t902\t102\t102\t2000\t2000\t1\t1\tchrR\tchrQ",
    "103\tA\tG\t903\t103\t103\t2000\t2000\t1\t1\tchrR\tchrQ",
    "104\tA\tG\t904\t104\t104\t2000\t2000\t1\t1\tchrR\tchrQ",
    "104\tA\tG\t905\t104\t104\t2000\t2000\t1\t1\tchrR\tchrQ",
    "110\tA\t.\t910\t110\t110\t2000\t2000\t1\t1\tchrR\tchrQ",
    "111\tA\tN\t911\t111\t111\t2000\t2000\t1\t1\tchrR\tchrQ"
  ), c(
    "chrR\t101\tzero\tA\tG\t.\tPASS\tAF=0",
    "chrR\t102\tone\tA\tG\t.\tPASS\tAF=1",
    "chrR\t103\tmismatch\tA\tT\t.\tPASS\tAF=.5",
    "chrR\t104\tmultiple_mappings\tA\tG\t.\tPASS\tAF=.5",
    "chrR\t105\tmissing_frequency\tA\tG\t.\tPASS\tDP=10",
    "chrR\t106\tinvalid_frequency\tA\tG\t.\tPASS\tAF=1.2",
    "chrR\t107\tmulti_alt\tA\tG,T\t.\tPASS\tAF=.4,.5",
    "chrR\t108\tindel\tAC\tA\t.\tPASS\tAF=.5",
    "chrR\t109\tunmapped\tA\tG\t.\tPASS\tAF=.5",
    "chrR\t110\tmummer_indel\tA\tG\t.\tPASS\tAF=.5",
    "chrR\t111\tmummer_ambiguous\tA\tG\t.\tPASS\tAF=.5",
    "chrR\t112\tmulti_af\tA\tG\t.\tPASS\tAF=.1,.9",
    "chrR\t113\tduplicate_af\tA\tG\t.\tPASS\tAF=.5;AF=.5",
    "chrR\t114\tlong_alt\tA\tGT\t.\tPASS\tAF=.5",
    "unrelated\t101\tother_chr\tA\tG\t.\tPASS\tAF=.5"
  ))
  warnings <- character()
  x <- withCallingHandlers(af_data_prepare(fixture), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_length(warnings, 1)
  expect_match(warnings, "12")
  expect_equal(query_allele_frequency(x)$af, c(0, 1))
  excluded <- query_allele_frequency(x, component = "excluded")
  expect_equal(excluded$source_row, 3:14)
  expect_identical(excluded$reason, c(
    "allele_mismatch", "ambiguous_mapping", "missing_af", "invalid_af",
    "multiallelic", "non_snp", "unmapped", "non_snp", "non_snp",
    "invalid_af", "invalid_af", "non_snp"
  ))
  expect_equal(sum(query_allele_frequency(x, component = "summary")$n), 14)
})

test_that("reusing a SNP source supports independent named preparations and custom INFO fields", {
  fixture <- af_data_fixture(vcf_rows = "chrR\t101\t.\tA\tG\t.\tPASS\tAF=.25;POP.AF=.75")
  x <- af_data_prepare(fixture, "default")
  x <- prepare_allele_frequency(x, "population", "snps", "custom", "POP.AF")
  expect_error(query_allele_frequency(x), "preparation name")
  expect_equal(query_allele_frequency(x, "default")$af, .25)
  expect_equal(query_allele_frequency(x, "custom")$af, .75)
})

test_that("empty SNP or VCF records produce typed empty preparation tables", {
  empty_snps <- af_data_fixture(character())
  x <- af_data_prepare(empty_snps)
  expect_equal(nrow(query_snps(x)), 0L)
  expect_identical(query_allele_frequency(x)$af, numeric())
  expect_identical(query_allele_frequency(x, component = "excluded")$reason, character())

  empty_vcf <- af_data_fixture(vcf_rows = character())
  expect_identical(query_allele_frequency(af_data_prepare(empty_vcf))$af, numeric())
})

test_that("replacing a source or clearing its cache invalidates the preparation", {
  fixture <- af_data_fixture()
  x <- af_data_prepare(fixture)
  expect_error(
    query_allele_frequency(add_snp_annotation(x, af_data_import(fixture))),
    "prepare_allele_frequency"
  )

  changed_pair <- fixture$pair
  changed_pair@data$track <- "new track"
  expect_error(
    query_allele_frequency(add_pairwise_alignment(x, changed_pair)),
    "prepare_allele_frequency"
  )

  changed_vcf <- fixture$vcf
  changed_vcf@metadata$cohort <- "replacement"
  ref <- add_annotation(individuals(x)[["ref"]], changed_vcf)
  expect_error(
    query_allele_frequency(add_individual(x, ref)),
    "prepare_allele_frequency"
  )
})

test_that("changes to VCF and show-snps inputs invalidate prepared records", {
  fixture <- af_data_fixture()
  x <- af_data_prepare(fixture)
  cat("chrR\t102\t.\tA\tG\t.\tPASS\tAF=.75\n", file = fixture$vcf_file, append = TRUE)
  expect_error(query_allele_frequency(x), "prepare_allele_frequency")

  fixture <- af_data_fixture()
  x <- af_data_prepare(fixture)
  cat("102\tA\tG\t902\t102\t102\t2000\t2000\t1\t1\tchrR\tchrQ\n",
      file = fixture$snp_file, append = TRUE)
  expect_error(query_allele_frequency(x), "prepare_allele_frequency")
  expect_error(
    prepare_allele_frequency(x, "population", "snps"),
    "import"
  )
})

test_that("cache payload corruption is detected before inspection", {
  x <- af_data_prepare(af_data_fixture())
  source <- snp_annotations(x)[["snps"]]
  source@plot_cache$allele_frequency[["allele_frequency"]]$variants$af <- .99
  x@metadata$ggexon_snp_annotations[["snps"]] <- source
  expect_error(query_allele_frequency(x), "prepare_allele_frequency")
})

test_that("typed VCF positions match large show-snps coordinates across chromosomes", {
  fixture <- af_data_fixture(c(
    "100000000\tA\tG\t901\t101\t101\t200000000\t2000\t1\t1\tchrR\tchrQ",
    "101\tA\tG\t909\t101\t101\t2000\t2000\t1\t1\tchr2\tchrQ"
  ), c(
    "chr2\t101\tsecond\tA\tG\t.\tPASS\tAF=.75",
    "chrR\t100000000\tlarge\tA\tG\t.\tPASS\tAF=.25"
  ))
  rows <- query_allele_frequency(af_data_prepare(fixture))
  expect_identical(rows$ID, c("large", "second"))
  expect_equal(rows$tstart, c(100000000, 101))
  expect_equal(rows$qstart, c(901, 909))
})

test_that("preparation queries indexed VCFs on chromosomes with matching records", {
  fixture <- af_data_fixture(c(
    "101\tA\tG\t901\t101\t101\t2000\t2000\t1\t1\tchrR\tchrQ",
    "101\tA\tG\t902\t101\t101\t2000\t2000\t1\t1\tchrMissing\tchrQ"
  ))
  bgzf <- withr::local_tempfile(fileext = ".vcf.gz")
  withr::defer(unlink(paste0(bgzf, ".tbi")))
  Rsamtools::bgzip(fixture$vcf_file, dest = bgzf)
  index <- Rsamtools::indexTabix(bgzf, format = "vcf")
  indexed <- SynVCFAnnotation("population", bgzf, index_file = index)
  cached <- query_variants(indexed, "chrR", 1L, 536870912L)
  cached$INFO <- "AF=.75"
  indexed@region_cache[["chrR:1:536870912"]] <- cached
  ref <- add_annotation(individuals(fixture$x)[["ref"]], indexed)
  fixture$x <- add_individual(fixture$x, ref)
  expect_equal(query_allele_frequency(af_data_prepare(fixture))$af, .25)
})

test_that("an in-memory SNP source remains reusable across separate VCF annotations", {
  fixture <- af_data_fixture()
  source <- SynSNPAnnotation("provided", "in-memory", "ref", "query", "pair", snps = data.frame(
    reference_chr = "chrR", reference_pos = 101, reference_base = "A",
    query_chr = "chrQ", query_pos = 901, query_base = "G", strand = "+"
  ))
  other <- withr::local_tempfile(fileext = ".vcf")
  writeLines(c(
    "##fileformat=VCFv4.2",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
    "chrR\t101\tsecond\tA\tG\t.\tPASS\tAF=.75"
  ), other)
  ref <- add_annotation(individuals(fixture$x)[["ref"]], SynVCFAnnotation("other", other))
  x <- add_individual(fixture$x, ref) |> add_snp_annotation(source)
  out <- prepare_allele_frequency(x, "population", "provided", "original") |>
    prepare_allele_frequency("other", "provided", "second")
  expect_identical(snp_annotations(x), list(provided = source))
  expect_equal(query_allele_frequency(out, "original")$af, .25)
  expect_equal(query_allele_frequency(out, "second")$af, .75)
})

test_that("preparation uses the track of an unloaded ODGI pair", {
  fixture <- af_data_fixture()
  pair <- SynPairAlignment(
    "pair", "query", "ref", "alignment.og", format = "odgi",
    metadata = list(odgi_alignment = "graph")
  )
  fixture$x <- add_pairwise_alignment(fixture$x, pair)
  expect_identical(
    query_allele_frequency(af_data_prepare(fixture))$track,
    "link_graph__query__ref"
  )
})

test_that("preparation refreshes frequency from the VCF file despite a legacy region cache", {
  fixture <- af_data_fixture()
  cached <- query_variants(fixture$vcf, "chrR", 1L, .Machine$integer.max)
  cached$INFO <- "AF=.75"
  fixture$vcf@region_cache[["chrR:1:2147483647"]] <- cached
  ref <- add_annotation(individuals(fixture$x)[["ref"]], fixture$vcf)
  fixture$x <- add_individual(fixture$x, ref)
  x <- af_data_prepare(fixture)
  expect_equal(query_allele_frequency(x)$af, .25)

  writeLines(c(
    "##fileformat=VCFv4.2",
    "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO",
    "chrR\t101\trs1\tA\tG\t60\tPASS\tAF=.625"
  ), fixture$vcf_file)
  expect_error(query_allele_frequency(x), "prepare_allele_frequency")
  refreshed <- prepare_allele_frequency(x, "population", "snps")
  expect_equal(query_allele_frequency(refreshed)$af, .625)
  expect_identical(get_annotation(individuals(refreshed)[["ref"]], "population"), fixture$vcf)
})

test_that("the preparation registry preserves unrelated species metadata", {
  for (user_value in list("user notes", list(allele_frequency = list(snps = "user notes")))) {
    fixture <- af_data_fixture()
    user_metadata <- list(
      allele_frequency = user_value,
      allele_frequency_other = "other notes",
      .ggexon_allele_frequency_other = "private-looking notes"
    )
    fixture$x@metadata <- user_metadata

    expect_error(query_allele_frequency(fixture$x), "No allele-frequency preparations")
    x <- af_data_prepare(fixture)
    expect_equal(query_allele_frequency(x)$af, .25)
    expect_identical(x@metadata[names(user_metadata)], user_metadata)
  }
})
