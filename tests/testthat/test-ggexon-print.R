test_that("printing a ggexon plot does not require attaching grid", {
  plot <- ggexon() +
    ggplot2::geom_point(
      data = data.frame(x = 1, y = 1),
      ggplot2::aes(x, y)
    )

  expect_false("package:grid" %in% search())
  expect_silent(print(plot, newpage = FALSE))
})
