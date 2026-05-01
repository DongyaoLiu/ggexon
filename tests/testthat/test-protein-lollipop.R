test_that("protein_lollipop_data spreads dense mutation labels and anchors domains", {
  mutations <- data.frame(
    position = c(20, 5, 8),
    ref = c("A", "M", "K"),
    alt = c("V", "T", "R"),
    blosum = c(0, -1, 2),
    stringsAsFactors = FALSE
  )
  domains <- data.frame(
    start = c(1, 15),
    end = c(10, 25),
    Domain = c("C2H2", "Coil"),
    ymin = c(0.35, 0.37),
    ymax = c(0.65, 0.63),
    stringsAsFactors = FALSE
  )

  prepared <- protein_lollipop_data(
    mutations = mutations,
    domains = domains,
    position = "position",
    ref = "ref",
    alt = "alt",
    score = "blosum",
    protein_length = 30,
    spread_threshold = 7,
    stem_points = 5
  )

  expect_s3_class(prepared, "ggexon_lollipop_data")
  expect_equal(prepared$mutations$lollipop_x, c(20, 5, 12))
  expect_equal(prepared$mutations$lollipop_label, c("A20V", "M5T", "K8R"))
  expect_equal(prepared$mutations$lollipop_stem_y, c(0.63, 0.65, 0.65))
  expect_equal(prepared$backbone$xmax, 30)
  expect_equal(nrow(prepared$stems), nrow(mutations) * 5)
})

test_that("geom_protein_lollipop builds ggplot layers", {
  mutations <- data.frame(
    position = c(5, 8, 20),
    mutation = c("M5T", "K8R", "A20V"),
    blosum = c(-1, 2, 0),
    stringsAsFactors = FALSE
  )
  domains <- data.frame(
    start = c(1, 15),
    end = c(10, 25),
    Domain = c("C2H2", "Coil"),
    stringsAsFactors = FALSE
  )

  plot_obj <- ggplot2::ggplot() +
    geom_protein_lollipop(
      mutations = mutations,
      domains = domains,
      protein_length = 30,
      score = "blosum",
      stem_points = 5
    )

  build <- ggplot2::ggplot_build(plot_obj)

  expect_length(build$data, 4)
  expect_equal(nrow(build$data[[1L]]), nrow(mutations) * 5)
  expect_equal(nrow(build$data[[2L]]), 1)
  expect_equal(nrow(build$data[[3L]]), 2)
  expect_equal(nrow(build$data[[4L]]), nrow(mutations))
})

test_that("protein_lollipop_data supports empty mutation tables with domains", {
  mutations <- data.frame(
    position = numeric(),
    mutation = character(),
    stringsAsFactors = FALSE
  )
  domains <- data.frame(
    start = 1,
    end = 10,
    Domain = "C2H2",
    stringsAsFactors = FALSE
  )

  prepared <- protein_lollipop_data(
    mutations = mutations,
    domains = domains,
    protein_length = 20
  )

  expect_equal(nrow(prepared$mutations), 0)
  expect_equal(nrow(prepared$stems), 0)
  expect_equal(prepared$backbone$xmax, 20)
})
