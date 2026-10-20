# Pack equal-width heads while preserving the order of every anchor.
.gene_lollipop_pack <- function(anchor_mm, width_mm, panel_width_mm,
                                 gap_mm = .8, max_shift_mm = 25,
                                 max_rows = Inf) {
  width_mm <- .genebox_positive_number(width_mm, "width_mm")
  panel_width_mm <- .genebox_positive_number(panel_width_mm, "panel_width_mm")
  if (!is.numeric(anchor_mm) || any(!is.finite(anchor_mm)))
    stop("All anchors must be finite numbers.", call. = FALSE)
  for (arg in c("gap_mm", "max_shift_mm")) {
    v <- get(arg)
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 0)
      stop("`", arg, "` must be one nonnegative finite number.", call. = FALSE)
  }
  if (!is.numeric(max_rows) || length(max_rows) != 1L || is.na(max_rows) ||
      max_rows < 1 || (is.finite(max_rows) && max_rows != floor(max_rows)))
    stop("`max_rows` must be a positive integer or Inf.", call. = FALSE)
  if (width_mm > panel_width_mm)
    stop("Head width exceeds panel width; increase the output width.", call. = FALSE)
  n <- length(anchor_mm)
  if (!n) return(data.frame(anchor_x_mm = numeric(), head_x_mm = numeric(),
                            row = integer(), left_mm = numeric(), right_mm = numeric()))
  ord <- order(anchor_mm, seq_len(n))
  a <- anchor_mm[ord]
  lo <- pmax(width_mm / 2, a - max_shift_mm)
  hi <- pmin(panel_width_mm - width_mm / 2, a + max_shift_mm)
  sep <- width_mm + gap_mm
  for (k in seq_len(min(n, max_rows))) {
    early <- lo
    for (i in seq_len(n)) {
      if (i > 1L) early[i] <- max(early[i], early[i - 1L])
      if (i > k) early[i] <- max(early[i], early[i - k] + sep)
    }
    if (any(early > hi + 1e-8)) next
    late <- hi
    for (i in rev(seq_len(n))) {
      if (i < n) late[i] <- min(late[i], late[i + 1L])
      if (i + k <= n) late[i] <- min(late[i], late[i + k] - sep)
    }
    if (any(early > late + 1e-8)) next
    x <- pmin(pmax(a, early), late)
    for (i in seq_len(n)) {
      if (i > 1L) x[i] <- max(x[i], x[i - 1L])
      if (i > k) x[i] <- max(x[i], x[i - k] + sep)
    }
    ans <- data.frame(anchor_x_mm = a, head_x_mm = x,
                      row = (seq_len(n) - 1L) %% k + 1L,
                      left_mm = x - width_mm / 2, right_mm = x + width_mm / 2)
    return(ans[order(ord), , drop = FALSE])
  }
  stop("Gene heads cannot fit: increase panel width, max_shift_mm or max_rows.", call. = FALSE)
}

# Consecutive display-ordered genes form columns read top to bottom.
# The gutter to each column's left carries ordered, noncrossing leader lanes.
.gene_lollipop_columns <- function(anchor_mm, width_mm, panel_width_mm,
    gap_mm = .8, max_shift_mm = 25, max_rows = Inf,
    lane_spacing_mm = .65, lane_clearance_mm = .5, head_row = NULL) {
  empty <- .gene_lollipop_pack(numeric(), width_mm, panel_width_mm,
                               gap_mm, max_shift_mm, max_rows)
  .genebox_positive_number(lane_spacing_mm, "lane_spacing_mm")
  .genebox_positive_number(lane_clearance_mm, "lane_clearance_mm")
  if (!is.numeric(anchor_mm) || any(!is.finite(anchor_mm)))
    stop("All anchors must be finite numbers.", call. = FALSE)
  n <- length(anchor_mm)
  if (!is.null(head_row) && !all(is.na(head_row))) {
    if (!is.numeric(head_row) || length(head_row) != n ||
        any(!is.finite(head_row)) || any(head_row < 1 | head_row != floor(head_row)))
      stop("`head_row` must supply a positive integer for every gene.", call. = FALSE)
    if (max(head_row) > max_rows)
      stop("Fixed head rows cannot fit: increase panel height, headroom or max_rows.", call. = FALSE)
    return(.gene_lollipop_fixed_columns(anchor_mm, width_mm, panel_width_mm,
      gap_mm, max_shift_mm, lane_spacing_mm, lane_clearance_mm, head_row))
  }
  if (!n) {
    empty$column <- empty$display_order <- integer()
    empty$channel_x_mm <- numeric()
    return(empty)
  }
  ord <- order(anchor_mm, seq_len(n))
  a <- anchor_mm[ord]
  for (k in seq_len(min(n, max_rows))) {
    column <- (seq_len(n) - 1L) %/% k + 1L
    slot <- (seq_len(n) - 1L) %% k + 1L
    groups <- split(seq_len(n), column)
    gutter <- (k - 1L) * lane_spacing_mm + 2 * lane_clearance_mm
    separation <- width_mm + gutter + gap_mm
    lo <- vapply(groups, function(i) max(width_mm/2 + gutter,
      max(a[i]) - max_shift_mm), numeric(1))
    hi <- vapply(groups, function(i) min(panel_width_mm - width_mm/2,
      min(a[i]) + max_shift_mm), numeric(1))
    early <- lo
    nc <- length(groups)
    if (nc > 1L) for (i in 2:nc) early[i] <- max(early[i], early[i-1L] + separation)
    if (any(early > hi + 1e-8)) next
    late <- hi
    if (nc > 1L) for (i in (nc-1L):1L) late[i] <- min(late[i], late[i+1L] - separation)
    if (any(early > late + 1e-8)) next
    preferred <- vapply(groups, function(i) mean(a[i]), numeric(1))
    cx <- pmin(pmax(preferred, early), late)
    if (nc > 1L) for (i in 2:nc) cx[i] <- max(cx[i], cx[i-1L] + separation)
    head <- unname(cx[column])
    left <- head - width_mm/2
    ans <- data.frame(anchor_x_mm = a, head_x_mm = head,
      row = k - slot + 1L, left_mm = left, right_mm = head + width_mm/2,
      column = column, display_order = seq_len(n),
      channel_x_mm = left - gutter + lane_clearance_mm + (slot-1L)*lane_spacing_mm)
    return(ans[order(ord), , drop = FALSE])
  }
  stop("Ordered gene columns cannot fit: increase panel width, max_shift_mm or max_rows.", call. = FALSE)
}

.gene_lollipop_stem_colour <- function(fill) {
  fill <- as.character(fill)
  fill[is.na(fill) | !nzchar(fill)] <- "grey60"
  rgb <- farver::decode_colour(fill, to = "rgb")
  rgb <- round(rgb * .6)
  to_upper_ascii(grDevices::rgb(rgb[,1], rgb[,2], rgb[,3], maxColorValue = 255))
}

# Fixed row identities retain blank slots and start a new column whenever
# display order would otherwise run upwards or put two heads in one cell.
.gene_lollipop_fixed_columns <- function(anchor_mm, width_mm, panel_width_mm,
    gap_mm, max_shift_mm, lane_spacing_mm, lane_clearance_mm, head_row) {
  n <- length(anchor_mm)
  ord <- order(anchor_mm, seq_len(n))
  a <- anchor_mm[ord]; rows <- as.integer(head_row[ord])
  # Earliest feasible last centre for each prefix / column count. Earlier
  # centres dominate later ones because the next column has only a minimum
  # separation constraint. Explore every allowed split, not just greedy runs.
  earliest <- matrix(Inf,nrow=n,ncol=n)
  starts <- matrix(NA_integer_,nrow=n,ncol=n)
  for (i in seq_len(n)) for (j in i:1L) {
    if (j < i && rows[j] <= rows[j+1L]) break
    gutter <- (i-j)*lane_spacing_mm + 2*lane_clearance_mm
    lower <- max(width_mm/2+gutter,a[i]-max_shift_mm)
    upper <- min(panel_width_mm-width_mm/2,a[j]+max_shift_mm)
    if (lower > upper+1e-8) break
    if (j == 1L) {
      earliest[i,1L] <- lower
      starts[i,1L] <- 1L
    } else for (count in 2:j) {
      centre <- max(lower,earliest[j-1L,count-1L]+width_mm+gutter+gap_mm)
      if (centre <= upper+1e-8 && centre < earliest[i,count]) {
        earliest[i,count] <- centre
        starts[i,count] <- j
      }
    }
  }
  feasible <- which(is.finite(earliest[n,]))
  if (!length(feasible))
    stop("Fixed-row gene columns cannot fit: increase panel width or max_shift_mm.", call. = FALSE)
  count <- feasible[1L]
  groups <- vector("list",count)
  i <- n
  for (col in count:1L) {
    j <- starts[i,col]
    groups[[col]] <- seq.int(j,i)
    i <- j-1L
  }
  column <- rep(seq_along(groups),lengths(groups))
  gutter <- (lengths(groups)-1L)*lane_spacing_mm + 2*lane_clearance_mm
  lo <- vapply(seq_along(groups), function(i) max(width_mm/2+gutter[i],
    max(a[groups[[i]]])-max_shift_mm), numeric(1))
  hi <- vapply(groups, function(i) min(panel_width_mm-width_mm/2,
    min(a[i])+max_shift_mm), numeric(1))
  early <- lo; late <- hi; nc <- length(groups)
  if (nc > 1L) for (i in 2:nc)
    early[i] <- max(early[i],early[i-1L]+width_mm+gutter[i]+gap_mm)
  if (nc > 1L) for (i in (nc-1L):1L)
    late[i] <- min(late[i],late[i+1L]-width_mm-gutter[i+1L]-gap_mm)
  if (any(early > late+1e-8))
    stop("Fixed-row gene columns cannot fit: increase panel width or max_shift_mm.", call. = FALSE)
  preferred <- vapply(groups, function(i) mean(a[i]), numeric(1))
  cx <- pmin(pmax(preferred,early),late)
  if (nc > 1L) for (i in 2:nc)
    cx[i] <- max(cx[i],cx[i-1L]+width_mm+gutter[i]+gap_mm)
  slot <- unlist(lapply(groups,seq_along),use.names=FALSE)
  head <- unname(cx[column]); left <- head-width_mm/2
  ans <- data.frame(anchor_x_mm=a,head_x_mm=head,row=rows,
    left_mm=left,right_mm=head+width_mm/2,column=column,display_order=seq_len(n),
    channel_x_mm=left-gutter[column]+lane_clearance_mm+(slot-1L)*lane_spacing_mm)
  ans[order(ord),,drop=FALSE]
}
