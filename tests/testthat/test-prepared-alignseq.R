prepared_alignseq_fixture <- function() {
  aligned <- c(query_dna = "ATCCTA-A", ref_dna = "AT--RGTA")
  anchors <- data.frame(sequence = names(aligned), start = c(700, 105),
    strand = c("+", "-"), track = c("other", "ref"), chr = c("chrQ", "chrR"))
  x <- add_individual(SynSpecies("pairwise"), SynIndividual(id = "ref"))
  x <- add_individual(x, SynIndividual(id = "other"))
  list(x = x, aligned = aligned, anchors = anchors)
}

test_that("prepared rows preserve aligned DNA evidence in native coordinates", {
  f <- prepared_alignseq_fixture()
  saved <- prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna",
    details = c(103, 104), flank = 1)
  before_query <- saved
  d <- query_alignseq(saved)
  raw <- alignseq_data(f$aligned, f$anchors, "ref_dna", details = c(103, 104), flank = 1)
  legacy_columns <- d[names(raw)]
  attr(legacy_columns, "ggexon_panel_role") <- NULL
  expect_identical(legacy_columns, raw)
  ref <- d[d$individual == "ref", ]
  expect_identical(ref$sequence, rep("ref_dna", 8))
  expect_identical(unique(d$reference), "ref_dna")
  expect_identical(d$track, d$individual)
  expect_equal(ref$x, c(105, 104, 103.5, 103.5, 103, 102, 101, 100))
  expect_equal(ref$position, c(105, 104, NA, NA, 103, 102, 101, 100))
  expect_identical(ref$event_type, c("match", "match", "insertion", "insertion",
    "ambiguous", "substitution", "deletion", "match"))
  expect_identical(ref$event_length[3:4], c(2L, 2L))
  expect_identical(which(!is.na(ref$detail_id)), 1:6)
  expect_false(any(d$boundary_after))
  expect_identical(unique(d$boundary_label), "")
  expect_identical(attr(d, "ggexon_panel_role"), "annotation")
  expect_identical(saved, before_query)
  expect_error(query_alignseq(f$x), "prepare_alignseq")

  entry <- plot_cache(individuals(saved)[["ref"]])$alignseq[["alignseq"]]
  expect_identical(unique(entry$data$individual), "ref")
  expect_identical(entry$recipe$aligned, f$aligned)
  expect_identical(entry$recipe$anchors, f$anchors)
  expect_identical(entry$recipe$reference, "ref_dna")
})

test_that("queries select individual, chromosome and native half-base intervals", {
  f <- prepared_alignseq_fixture()
  saved <- prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna")
  selected <- query_alignseq(saved, individual = "other",
    chr = "chrQ", start = 705.5, end = 706)
  expect_identical(selected$individual, c("other", "other"))
  expect_equal(selected$x, c(705.5, 706))
  expect_identical(selected$base, c("-", "A"))
  expect_equal(query_alignseq(saved, individual = "ref", end = 101)$x, c(101, 100))
  expect_equal(query_alignseq(saved, individual = "other", start = 706)$x, 706)
  empty <- query_alignseq(saved, individual = "other", chr = "chrR")
  expect_identical(empty, selected[FALSE, , drop = FALSE])
  expect_error(query_alignseq(saved, individual = "unknown"), "existing individuals")
  expect_error(query_alignseq(saved, start = 3, end = 2), "start.*end")
})

test_that("native boundaries mark their own sequence even on reverse strands", {
  f <- prepared_alignseq_fixture()
  boundaries <- data.frame(sequence = c("ref_dna", "query_dna"),
    position = c(103, 703), label = c("reverse boundary", "outside region"))
  saved <- prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna",
    regions = c(101, 103), boundaries = boundaries)
  d <- query_alignseq(saved)
  expect_identical(d$sequence[d$boundary_after], "ref_dna")
  expect_equal(d$position[d$boundary_after], 103)
  expect_identical(d$boundary_label[d$boundary_after], "reverse boundary")
  expect_true(all(d$boundary_label[!d$boundary_after] == ""))
  entry <- plot_cache(individuals(saved)[["ref"]])$alignseq[["alignseq"]]
  expect_identical(entry$recipe$boundaries, boundaries)

  boundaries$position[1] <- 103.5
  expect_error(prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna",
    boundaries = boundaries), "boundar.*integer")
  boundaries$position[1] <- 700
  expect_error(prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna",
    boundaries = boundaries), "boundar.*native")
  boundaries$sequence[1] <- "unknown"
  expect_error(prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna",
    boundaries = boundaries), "boundar.*sequence")
})

test_that("anchor tracks must identify existing individuals", {
  f <- prepared_alignseq_fixture()
  f$anchors$track[1] <- "unregistered"
  expect_error(prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna"),
    "anchors.*track.*existing individual")
  f$anchors$track <- NULL
  x <- add_individual(SynSpecies("names"), SynIndividual(id = "query_dna"))
  x <- add_individual(x, SynIndividual(id = "ref_dna"))
  d <- query_alignseq(prepare_alignseq(x, f$aligned, f$anchors, "ref_dna"))
  expect_identical(d$individual, d$sequence)
  f$anchors$start[1] <- 700.5
  expect_error(prepare_alignseq(x, f$aligned, f$anchors, "ref_dna"),
    "Anchor starts")
})

test_that("names are inferred on selected individuals and empty preparations are typed", {
  f <- prepared_alignseq_fixture()
  saved <- prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna", name = "whole")

  single <- individuals(saved)[["ref"]]
  f$anchors$track <- "ref"
  single <- prepare_alignseq(single, f$aligned, f$anchors, "ref_dna",
    name = "empty", regions = c(800, 801))
  both <- add_individual(
    add_individual(SynSpecies("both"), single), individuals(saved)[["other"]])

  expect_error(query_alignseq(both), "Multiple.*name")
  expect_error(query_alignseq(both, name = "unknown"), "Unknown.*prepare_alignseq")
  expect_equal(nrow(query_alignseq(both, individual = "other")), 8)
  expect_equal(nrow(query_alignseq(both, name = "whole")), 16)
  expect_equal(nrow(query_alignseq(both, name = "empty", individual = "other")), 0)
  expect_identical(query_alignseq(single, name = "empty"),
    query_alignseq(single, name = "whole")[FALSE, , drop = FALSE])

  single <- prepare_alignseq(single, f$aligned, f$anchors, "ref_dna", name = "local")
  local <- query_alignseq(single, name = "local")
  expect_identical(unique(local$individual), "ref")
  expect_identical(unique(local$sequence), c("ref_dna", "query_dna"))
})

test_that("a cleared prepared alignment requires preparation again", {
  f <- prepared_alignseq_fixture()
  saved <- prepare_alignseq(f$x, f$aligned, f$anchors, "ref_dna")
  single <- individuals(saved)[["ref"]]
  plot_cache(single) <- list()
  expect_error(query_alignseq(single), "cleared cache.*prepare_alignseq")
  expect_error(query_alignseq(single, "alignseq"), "cleared cache.*prepare_alignseq")
  expect_equal(nrow(query_alignseq(saved, individual = "ref")), 8)
})
