test_that("transcript backbone rectangles use exon height ratio", {
  transcript_df <- data.frame(
    x = c(10, 40),
    xend = c(110, -20),
    y_middle = c(5, 8),
    y_range = c(2, 4),
    linewidth = c(1, 1),
    fill = c("black", "grey50"),
    colour = c("black", "grey50"),
    stringsAsFactors = FALSE
  )

  rect_df <- ggexon:::add_transcripts_seq_rect(transcript_df, backbone_ratio = 0.1)

  expect_equal(rect_df$xmin, c(10, -20))
  expect_equal(rect_df$xmax, c(110, 40))
  expect_equal(rect_df$ymax - rect_df$ymin, c(0.2, 0.4), tolerance = 1e-9)
  expect_equal(rect_df$y_middle, transcript_df$y_middle)
  expect_true(all(is.na(rect_df$colour)))
  expect_true(all(rect_df$linewidth == 0))
})
