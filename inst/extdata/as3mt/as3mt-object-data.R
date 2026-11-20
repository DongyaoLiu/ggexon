# Example-specific exon labels. Genomic x coordinates
# remain unchanged; the plot's genomic scale supplies the shared intron map.
prepare_as3mt_example <- function(model) {
  stopifnot(methods::is(model, "SynSpecies"))
  labels <- do.call(rbind, lapply(ggexon::individual_names(model), function(track) {
    features <- as.data.frame(ggexon::query_features(model@individuals[[track]],
      feature_type = NULL, all = TRUE))
    features <- features[!is.na(features$exon_number), , drop = FALSE]
    groups <- split(seq_len(nrow(features)), paste(features$transcript_id, features$exon_number))
    do.call(rbind, lapply(groups, function(i) data.frame(
      id = as.character(features$transcript_id[i[1]]), exon = as.integer(features$exon_number[i[1]]),
      start = min(features$start[i]), end = max(features$end[i]), track = track,
      y = features$isoform_y[i[1]] - .35)))
  }))
  labels <- labels[order(match(labels$track, ggexon::individual_names(model)),
    -labels$y, labels$id, labels$exon), ]
  rownames(labels) <- NULL
  labels$x <- (labels$start + labels$end) / 2
  labels$y[labels$exon == 2] <- labels$y[labels$exon == 2] + .15
  labels$label <- paste0("E", labels$exon)
  model@metadata$as3mt$plot_data <- list(exon_labels = labels)
  model
}

# Retrieve the prepared exon labels for the annotation panels.
as3mt_plot_data <- function(model, component) {
  stopifnot(methods::is(model, "SynSpecies"))
  prepared <- as3mt_evidence(model)$plot_data
  if (is.null(prepared)) stop("Run prepare_as3mt_example() before querying plot views.")
  if (length(component) != 1L || !component %in% names(prepared))
    stop("Unknown AS3MT plot component.")
  out <- prepared[[component]]
  attr(out, "ggexon_panel_role") <- "annotation"
  out
}
