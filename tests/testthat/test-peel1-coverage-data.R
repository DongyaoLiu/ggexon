test_that("PEEL-1 coverage fixtures contain four raw tracks and four complete genes", {
  fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
  expect_true(nzchar(fixture_dir))

  manifest_path <- file.path(fixture_dir, "manifest.tsv")
  gtf_path <- file.path(
    fixture_dir,
    "WS285.ugt31-zeel1-peel1-nekl1.gtf"
  )
  expect_true(file.exists(manifest_path))
  expect_true(file.exists(gtf_path))

  manifest <- utils::read.delim(manifest_path, check.names = FALSE)
  expect_identical(
    as.character(manifest$strain),
    c("XZ1516", "ECA2091", "ECA701", "ECA2191")
  )
  expect_true(all(manifest$chr == "I"))
  expect_true(all(manifest$start == 2332338L))
  expect_true(all(manifest$end == 2373985L))
  expect_true(all(manifest$normalization == "None"))
  expect_true(all(manifest$bin_size == 1L))
  expect_true(all(manifest$bamCoverage_region == "I:2332337:2373985"))
  expect_true(all(nzchar(manifest$checksum)))

  bw_paths <- file.path(fixture_dir, manifest$bigwig)
  expect_true(all(file.exists(bw_paths)))
  expect_true(all(file.info(bw_paths)$size > 0L))
  expect_identical(
    unname(tools::md5sum(bw_paths)),
    as.character(manifest$checksum)
  )

  gtf_lines <- readLines(gtf_path, warn = FALSE)
  gene_ids <- c(
    "WBGene00021464", "WBGene00021463",
    "WBGene00077563", "WBGene00021461"
  )
  expect_true(all(vapply(gene_ids, function(id) {
    any(grepl(id, gtf_lines, fixed = TRUE))
  }, logical(1))))
  expect_false(any(grepl("WBGene", gtf_lines) &
    !Reduce(`|`, lapply(gene_ids, grepl, x = gtf_lines, fixed = TRUE))))

  queried <- lapply(bw_paths, function(path) {
    rtracklayer::import.bw(
      path,
      which = GenomicRanges::GRanges(
        "I",
        IRanges::IRanges(2332338L, 2373985L)
      )
    )
  })
  expect_true(all(vapply(queried, length, integer(1)) > 0L))
  expect_true(all(vapply(queried, function(gr) {
    all(as.numeric(S4Vectors::mcols(gr)$score) >= 0)
  }, logical(1))))
  raw_maxima <- vapply(queried, function(gr) {
    max(as.numeric(S4Vectors::mcols(gr)$score))
  }, numeric(1))
  expect_identical(
    unname(raw_maxima),
    c(187, 6115, 228, 192)
  )
  expect_true(all(vapply(queried, function(gr) {
    reduced <- GenomicRanges::reduce(gr)
    length(reduced) == 1L &&
      BiocGenerics::start(reduced)[[1L]] == 2332338L &&
      BiocGenerics::end(reduced)[[1L]] == 2373985L &&
      sum(BiocGenerics::width(reduced)) == 41648L
  }, logical(1))))
})
