#' Prepare anchored pairwise DNA differences
#'
#' Convert two already aligned DNA strings into an inspectable table for
#' [geom_alignseq()]. No alignment is inferred. Coordinates are one-based base
#' centres; gaps do not consume genomic bases and are anchored at the adjacent
#' half-base boundary. Insertions and deletions describe the other sequence
#' relative to the explicitly named reference, not evolutionary direction.
#'
#' @param aligned Named character vector of exactly two equal-length aligned
#'   DNA strings. Use `-` for gaps. IUPAC ambiguity codes are retained and marked
#'   `ambiguous`, rather than called verified substitutions.
#' @param anchors Data frame with one row per sequence and columns `sequence`,
#'   `start`, `strand`. `start` is the genomic coordinate of the first non-gap
#'   character in the supplied orientation; `strand` is `+` or `-`. Strings on
#'   the minus strand must already be oriented for comparison. Optional `track`,
#'   `chr`, and `y` columns control annotation-panel placement. Defaults are the
#'   sequence name, `NA`, and 1, respectively. Anchors describe contiguous DNA,
#'   not spliced transcript sequence.
#' @param reference Name of the reference in `aligned`.
#' @param regions Selected reference-coordinate windows: a numeric start/end
#'   pair or a data frame with `start`, `end`. `NULL` retains the whole alignment.
#'   A gap column is selected by its half-base reference anchor.
#' @param details Explicit reference-coordinate windows for DNA callouts, in
#'   the same format as `regions`. `NULL` adds no callouts. Nearby windows merge
#'   when their expanded alignment-column ranges overlap or touch. Detail is
#'   restricted to the selected regions and split wherever columns are omitted.
#' @param flank Nonnegative integer number of aligned columns added on each
#'   side of each detail window.
#' @return A data frame, one row per sequence per selected aligned column, with
#'   `x`, `y`, `track`, `position` (NA at gaps), `column`, `base`, `event_type`,
#'   `event_id`, `event_length`, `reference_x`, and `detail_id`. Contiguous indel
#'   columns form one event; each substitution is one event. Matching columns
#'   have no event ID. Rows also retain both aligned bases and sequence names
#'   for matched-column comparison. Empty selections return a typed empty table.
#' @export
#' @examples
#' dna <- c(human = "AC-GTAC", chimp = "ATCG-AC")
#' anchors <- data.frame(sequence = names(dna), start = c(100, 900), strand = "+")
#' d <- alignseq_data(dna, anchors, reference = "human",
#'   details = c(101, 103), flank = 1)
#' ggplot2::ggplot() + geom_alignseq(data = d) +
#'   ggplot2::facet_wrap(~track, scales = "free_x")
alignseq_data <- function(aligned, anchors, reference, regions = NULL,
                          details = NULL, flank = 3) {
  if (!is.character(aligned) || length(aligned) != 2L || anyNA(aligned) ||
      is.null(names(aligned)) || anyNA(names(aligned)) ||
      any(!nzchar(names(aligned))) || anyDuplicated(names(aligned))) {
    stop("aligned must contain exactly two uniquely named DNA strings.", call. = FALSE)
  }
  if (missing(reference) || length(reference) != 1L || is.na(reference) ||
      !reference %in% names(aligned)) stop("Name a reference present in aligned.", call. = FALSE)
  aligned <- to_upper_ascii(aligned[c(reference, setdiff(names(aligned), reference))])
  if (length(unique(nchar(aligned))) != 1L || any(nchar(aligned) == 0L))
    stop("Aligned DNA strings must have equal, nonzero lengths.", call. = FALSE)
  if (any(grepl("[^ACGTRYSWKMBDHVN-]", aligned)))
    stop("Use IUPAC DNA characters and '-' gaps only.", call. = FALSE)
  b <- lapply(aligned, function(s) strsplit(s, "", fixed = TRUE)[[1L]])
  if (any(b[[1]] == "-" & b[[2]] == "-"))
    stop("Remove double-gap alignment columns.", call. = FALSE)
  if (any(vapply(b, function(v) all(v == "-"), logical(1))))
    stop("Each DNA string must contain at least one genomic base.", call. = FALSE)
  if (!is.data.frame(anchors) || !all(c("sequence", "start", "strand") %in% names(anchors)) ||
      nrow(anchors) != 2L || anyDuplicated(anchors$sequence) ||
      !setequal(as.character(anchors$sequence), names(aligned)))
    stop("anchors must have one sequence/start/strand row for each DNA string.", call. = FALSE)
  anchors <- anchors[match(names(aligned), anchors$sequence), , drop = FALSE]
  if (!is.numeric(anchors$start) || any(!is.finite(anchors$start)) ||
      any(anchors$start < 1 | anchors$start != floor(anchors$start)))
    stop("Anchor starts must be positive integer genomic coordinates.", call. = FALSE)
  if (anyNA(anchors$strand) || any(!anchors$strand %in% c("+", "-")))
    stop("Anchor strand must be '+' or '-'.", call. = FALSE)
  if (!is.numeric(flank) || length(flank) != 1L || !is.finite(flank) || flank < 0 || flank != floor(flank))
    stop("flank must be a nonnegative integer.", call. = FALSE)
  if (!"track" %in% names(anchors)) anchors$track <- anchors$sequence
  if (!"chr" %in% names(anchors)) anchors$chr <- NA_character_
  if (!"y" %in% names(anchors)) anchors$y <- 1
  if (!is.numeric(anchors$y) || any(!is.finite(anchors$y)) || anyNA(anchors$track))
    stop("Anchor y must be finite and track must be present.", call. = FALSE)
  positions <- lapply(1:2, function(i) {
    step <- if (anchors$strand[i] == "+") 1 else -1
    counts <- cumsum(b[[i]] != "-")
    pos <- anchors$start[i] + step * (counts - 1)
    gap <- b[[i]] == "-"
    x <- pos
    x[gap] <- pos[gap] + step * .5
    pos[gap] <- NA_real_
    if (any(pos < 1, na.rm = TRUE)) stop("Anchor extends below genomic coordinate 1.", call. = FALSE)
    list(x = x, position = pos)
  })
  n <- length(b[[1]])
  type <- rep("match", n)
  type[b[[1]] != b[[2]]] <- "substitution"
  uncertain <- !(b[[1]] %in% c("A", "C", "G", "T", "-")) |
    !(b[[2]] %in% c("A", "C", "G", "T", "-"))
  type[uncertain] <- "ambiguous"
  type[b[[1]] == "-"] <- "insertion"
  type[b[[2]] == "-"] <- "deletion"
  id <- rep(NA_integer_, n)
  event <- 0L
  for (j in seq_len(n)) {
    if (type[j] == "match") next
    same_indel <- j > 1L && type[j] %in% c("insertion", "deletion") && type[j] == type[j - 1L]
    if (!same_indel) event <- event + 1L
    id[j] <- event
  }
  lengths <- rep(NA_integer_, n)
  if (event) lengths[!is.na(id)] <- as.integer(table(id)[as.character(id[!is.na(id)])])
  select <- .alignseq_window_mask(positions[[1]]$x, regions, default = TRUE)
  detail_id <- rep(NA_integer_, n)
  windows <- .alignseq_windows(details)
  intervals <- lapply(seq_len(nrow(windows)), function(i) {
    cols <- which(positions[[1]]$x >= windows$start[i] & positions[[1]]$x <= windows$end[i] & select)
    if (!length(cols)) return(NULL)
    c(max(1, min(cols) - flank), min(n, max(cols) + flank))
  })
  intervals <- Filter(Negate(is.null), intervals)
  if (length(intervals)) {
    ranges <- do.call(rbind, intervals)
    ranges <- ranges[order(ranges[, 1]), , drop = FALSE]
    merged <- list(ranges[1, ])
    if (nrow(ranges) > 1L) for (i in 2:nrow(ranges)) {
      k <- length(merged)
      if (ranges[i, 1] <= merged[[k]][2] + 1) merged[[k]][2] <- max(merged[[k]][2], ranges[i, 2])
      else merged[[k + 1L]] <- ranges[i, ]
    }
    for (i in seq_along(merged)) detail_id[seq.int(merged[[i]][1], merged[[i]][2])] <- i
  }
  # A selection can exclude intervening columns: never join those bases as
  # adjacent characters in a detail callout, even inside one requested window.
  old_detail <- detail_id
  detail_id[] <- NA_integer_
  previous <- NA_integer_
  next_detail <- 0L
  for (j in which(select & !is.na(old_detail))) {
    if (is.na(previous) || j != previous + 1L || old_detail[j] != old_detail[previous])
      next_detail <- next_detail + 1L
    detail_id[j] <- next_detail
    previous <- j
  }
  out <- do.call(rbind, lapply(1:2, function(i) data.frame(
    sequence = names(aligned)[i], reference = reference, query = names(aligned)[2],
    track = as.character(anchors$track[i]), chr = as.character(anchors$chr[i]),
    strand = as.character(anchors$strand[i]), x = positions[[i]]$x,
    y = anchors$y[i], position = positions[[i]]$position,
    column = seq_len(n), base = b[[i]], reference_base = b[[1]], query_base = b[[2]],
    reference_x = positions[[1]]$x, event_type = type, event_id = id,
    event_length = lengths, detail_id = detail_id, stringsAsFactors = FALSE)))
  out <- out[rep(select, 2), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.alignseq_windows <- function(windows) {
  if (is.null(windows)) return(data.frame(start = numeric(), end = numeric()))
  if (is.numeric(windows) && length(windows) == 2L) windows <- data.frame(start = windows[1], end = windows[2])
  if (!is.data.frame(windows) || !all(c("start", "end") %in% names(windows)) ||
      !is.numeric(windows$start) || !is.numeric(windows$end) ||
      any(!is.finite(windows$start) | !is.finite(windows$end) | windows$start > windows$end))
    stop("Windows must contain finite start/end coordinates with start <= end.", call. = FALSE)
  windows
}

.alignseq_window_mask <- function(x, windows, default) {
  if (is.null(windows)) return(rep(default, length(x)))
  windows <- .alignseq_windows(windows)
  keep <- rep(FALSE, length(x))
  for (i in seq_len(nrow(windows))) keep <- keep | (x >= windows$start[i] & x <= windows$end[i])
  keep
}
