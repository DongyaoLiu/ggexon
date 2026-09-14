test_that("PAF imports expose one-based closed coordinates at boundaries", {
  path <- tempfile(fileext = ".paf")
  writeLines(paste(c(
    "q", 10, 0, 1, "+", "t", 10, 0, 1, 1, 1, 60, "cg:Z:1M"
  ), collapse = "\t"), path)
  pair <- SynPairAlignment("q_to_t", "q_species", "t_species", path, "paf")

  coarse <- pairwise_alignment_data(pair)
  detail <- pairwise_alignment_data(pair, cigar = TRUE)
  boundary <- pairwise_alignment_data(pair, subset = c(q_species = "q:1-1"))
  outside <- pairwise_alignment_data(pair, subset = c(q_species = "q:2-2"))

  expect_identical(coarse[c("qstart", "qend", "tstart", "tend")],
                   data.frame(qstart = 1L, qend = 1L, tstart = 1L, tend = 1L))
  expect_identical(detail$qstart, 1L)
  expect_identical(detail$qend, 1L)
  expect_identical(detail$qstart_raw, 0L)
  expect_identical(detail$qend_raw, 1L)
  expect_identical(nrow(boundary), 1L)
  expect_identical(nrow(outside), 0L)
})

test_that("PSL coarse and expanded rows expose one-based closed coordinates", {
  path <- tempfile(fileext = ".psl")
  writeLines(c(
    paste(c(1, 0, 0, 0, 0, 0, 0, 0, "++", "q", 10, 0, 1,
            "t", 10, 0, 1, 1, "1,", "0,", "0,"), collapse = "\t"),
    paste(c(1, 0, 0, 0, 0, 0, 0, 0, "+-", "q", 10, 0, 1,
            "t", 10, 0, 1, 1, "1,", "0,", "9,"), collapse = "\t")
  ), path)

  coarse <- read_pairwise_psl(path)
  detail <- read_pairwise_psl(path, more = TRUE)

  expect_identical(coarse$qstart, c(1L, 1L))
  expect_identical(coarse$qend, c(1L, 1L))
  expect_identical(coarse$tstart, c(1L, 1L))
  expect_identical(coarse$tend, c(1L, 1L))
  expect_identical(detail$qstart, c(1L, 1L))
  expect_identical(detail$tstart, c(1L, 1L))
  expect_identical(detail$qstart_raw, c(0L, 0L))
  expect_identical(detail$tstart_raw, c(0L, 9L))
})

test_that("pre-normalized cached pairwise data is not converted again", {
  cached <- data.frame(
    qchr = "q", qlen = 10L, qstart = 1L, qend = 1L, strand = "+",
    tchr = "t", tlen = 10L, tstart = 1L, tend = 1L,
    nmatch = 1L, alen = 1L, mapq = 60L,
    stringsAsFactors = FALSE
  )
  pair <- SynPairAlignment(
    "cached",
    query_individual = "q_species",
    target_individual = "t_species",
    file = "unused.paf",
    format = "paf",
    data = cached
  )

  result <- pairwise_alignment_data(pair)
  expect_identical(result$qstart, 1L)
  expect_identical(result$qend, 1L)
  expect_identical(result$tstart, 1L)
  expect_identical(result$tend, 1L)
})
