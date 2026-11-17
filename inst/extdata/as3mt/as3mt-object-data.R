# Example-specific preparation of annotation overlays. Genomic x coordinates
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
      start = min(features$start[i]), end = max(features$end[i]), track = track)))
  }))
  labels <- labels[order(match(labels$id, c("human_full", "human_short", "chimp")), labels$exon), ]
  rownames(labels) <- NULL
  labels$x <- (labels$start + labels$end) / 2
  labels$y <- ifelse(labels$id == "human_full", 2.45, 1.35)
  labels$y[labels$exon == 2] <- labels$y[labels$exon == 2] + .25
  labels$label <- paste0("E", labels$exon)
  a <- as3mt_evidence(model)$vntr
  dna <- ggexon::alignseq_data(a$aligned, a$anchors, reference = "human",
    regions = a$regions, details = a$details, flank = a$flank)
  dna$y <- ifelse(dna$sequence == "human", 2.65, 1.5)
  dna$boundary_after <- !is.na(dna$position) & dna$position == a$donor_after[dna$sequence]
  dna$boundary_label <- ifelse(dna$boundary_after, "E1 donor | intron 1", "")
  # Keep the validated donor-anchored deletion marker; substitutions remain
  # visible in the DNA callout without being labelled as causal SNVs.
  dna$event_id[dna$event_type == "substitution"] <- NA_integer_
  rows <- ggexon::alignseq_data(a$aligned, a$anchors, reference = "human", regions = a$details)
  rows$y <- ifelse(rows$sequence == "human", 2.8, 1.7)
  rows$boundary_after <- !is.na(rows$position) & rows$position == a$donor_after[rows$sequence]
  rows$boundary_label <- ifelse(rows$boundary_after, "E1 donor | intron 1", "")
  repeats <- rbind(
    data.frame(track = "human", start = a$human_units$start, end = a$human_units$end, y = 1.8),
    data.frame(track = "human", start = a$human_units$start, end = a$human_units$end, y = .7),
    data.frame(track = "chimp", start = a$chimp_unit_start, end = a$chimp_unit_end, y = .7))
  donors <- data.frame(track = c("human", "chimp"), x = unname(a$donor_after) + .5,
    y = c(1.48, .38), yend = c(2.12, 1.02))
  model@metadata$as3mt$plot_data <- list(exon_labels = labels,
    dna_markers = dna, dna_rows = rows, repeats = repeats, donors = donors)
  model
}

# These queries expose prepared state; they do not accept independent tables.
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
