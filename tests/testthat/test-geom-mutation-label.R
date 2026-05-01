test_that("geom_mutation_label builds explicit data-frame label layers", {
  mutations <- data.frame(
    position = c(5, 8, 20),
    mutation = c("M5T", "", "A20V"),
    stringsAsFactors = FALSE
  )

  plot_obj <- ggplot2::ggplot() +
    geom_mutation_label(
      mutations = mutations,
      label = "mutation",
      spread_threshold = 7,
      mutation_y = 1,
      label_nudge_y = 0.25
    )

  build <- ggplot2::ggplot_build(plot_obj)

  expect_length(build$data, 1)
  expect_equal(nrow(build$data[[1L]]), 2)
  expect_equal(build$data[[1L]]$label, c("M5T", "A20V"))
  expect_equal(build$data[[1L]]$y, c(1.25, 1.25))
})

test_that("geom_mutation_label dispatches from SynSpecies mutation annotations", {
  mutation_file <- tempfile(fileext = ".tsv")
  writeLines(
    c(
      "gene\tevent_type\tmutation_hash_or_notation\tlength\tsample_count\tstrains",
      "zina-1\tsubstitution\tC#316#H\t1\t2\tind1,ind2",
      "zina-1\tsubstitution\tD#219#I\t1\t1\tind2"
    ),
    mutation_file
  )

  sp <- SynSpecies("test")
  sp <- add_protein_mutation_annotation(sp, mutation_file)

  plot_obj <- ggexon(sp) +
    geom_mutation_label(
      individual = "ind2",
      genes = "zina-1",
      label = "mutation",
      mutation_y = 1,
      label_nudge_y = 0.3
    )

  build <- ggexon_build(plot_obj)
  label_layer <- build@data[[1L]]

  expect_equal(nrow(label_layer), 2)
  expect_setequal(label_layer$label, c("C316H", "D219I"))
  expect_true(all(as.character(label_layer$track) == "ind2"))
})
