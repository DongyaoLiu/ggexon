test_that("ggexon_output_size() stores a plot-level size contract", {
  spec <- ggexon_output_size(12, 8, units = "in")

  expect_s3_class(spec, "ggexon_output_size_spec")
  expect_equal(spec$width_mm, 304.8)
  expect_equal(spec$height_mm, 203.2)

  p <- ggexon() + ggexon_output_size(100, 50, units = "mm")
  expect_s3_class(p@output_size, "ggexon_output_size_spec")
  expect_equal(p@output_size$width_mm, 100)
})

test_that("ggexon_output_size() validates input and only adds to ggexon plots", {
  expect_error(ggexon_output_size(0, 8), "positive")
  expect_error(ggexon_output_size(8, -1), "positive")
  expect_error(ggexon_output_size(100, 100, units = "px", dpi = 0), "positive")

  expect_error(
    ggplot2::ggplot() + ggexon_output_size(4, 3),
    "can only be added to a ggexon plot"
  )
})
