test_that("shipped manifests and provenance contain no local absolute paths", {
  root <- system.file("extdata", package = "ggexon")
  expect_true(nzchar(root))
  files <- list.files(
    root,
    pattern = "(manifest|provenance).*\\.tsv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  expect_gt(length(files), 0L)

  for (path in files) {
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    posix_absolute <- grepl(
      "(^|[[:space:]\\\"'=])/(?!/)", text, perl = TRUE
    )
    windows_absolute <- grepl(
      "(^|[[:space:]\\\"'=])[A-Za-z]:[/\\\\]", text, perl = TRUE
    )
    expect_false(posix_absolute, info = path)
    expect_false(windows_absolute, info = path)
  }
})
