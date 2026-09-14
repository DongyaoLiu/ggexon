test_that("package version floors and dependency fields match the public contract", {
  description_path <- system.file("DESCRIPTION", package = "ggexon")
  expect_true(nzchar(description_path))
  description <- read.dcf(description_path)[1L, ]
  depends <- trimws(strsplit(description[["Depends"]], ",")[[1L]])
  imports <- trimws(strsplit(description[["Imports"]], ",")[[1L]])

  expect_identical(depends, "R (>= 4.1)")
  expect_true("ggplot2 (>= 4.0.0)" %in% imports)
  expect_true(all(c(
    "methods", "gtable", "ggforce", "dplyr", "rtracklayer", "grid",
    "tidyr", "scales", "reshape2", "rlang", "vctrs", "S7"
  ) %in% imports))
})
