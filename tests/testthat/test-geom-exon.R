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

test_that("transcript backbone aesthetics can be fixed independently", {
  backbone_df <- data.frame(
    fill = c("red", "blue"),
    colour = c("red", "blue"),
    stringsAsFactors = FALSE
  )

  inherited_df <- ggexon:::.apply_transcript_backbone_aes(backbone_df)
  fixed_df <- ggexon:::.apply_transcript_backbone_aes(
    backbone_df,
    fill = "grey82",
    colour = NA
  )
  line_df <- ggexon:::.apply_transcript_backbone_aes(
    backbone_df,
    fill = "grey82",
    use_fill_as_colour = TRUE
  )
  empty_df <- ggexon:::.apply_transcript_backbone_aes(
    backbone_df[0L, ],
    fill = "grey82",
    colour = NA
  )

  expect_identical(inherited_df, backbone_df)
  expect_equal(fixed_df$fill, c("grey82", "grey82"))
  expect_true(all(is.na(fixed_df$colour)))
  expect_equal(line_df$fill, c("grey82", "grey82"))
  expect_equal(line_df$colour, c("grey82", "grey82"))
  expect_equal(nrow(empty_df), 0L)
})
