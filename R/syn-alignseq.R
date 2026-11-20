# Resolve recipients from annotation panels, including custom track names.
syn_alignseq_data <- function(x, layer) {
  params <- layer$geom_params
  context <- layer$syn_plot_context
  ids <- names(.prepared_alignseq_individuals(x))
  tracks <- data.frame(track = ids, individual = ids)
  if (methods::is(x, "SynSpecies") && !is.null(species_layout(x))) {
    tracks <- effective_panel_windows(x)[c("track", "individual")]
    tracks <- unique(tracks[tracks$individual %in% ids, , drop = FALSE])
  }
  selected <- params$species %||% context$annotation_species_order
  if (length(selected)) {
    tracks <- tracks[tracks$individual %in% selected | tracks$track %in% selected, , drop = FALSE]
  }
  rows <- query_alignseq(x, name = params$alignment, individual = unique(tracks$individual))
  if (length(params$chr) == 1L) rows$chr[is.na(rows$chr)] <- params$chr
  windows <- context$annotation_windows
  tables <- lapply(seq_len(nrow(tracks)), function(i) {
    id <- tracks$individual[i]
    track <- tracks$track[i]
    window <- windows[[track]] %||% windows[[id]]
    d <- rows[rows$individual == id, , drop = FALSE]
    chr <- window$chr
    if (anyNA(d$chr) && sum(tracks$individual == id) > 1L) {
      chromosomes <- vapply(tracks$track[tracks$individual == id], function(t)
        windows[[t]]$chr %||% NA_character_, character(1))
      if (anyNA(chromosomes) || length(unique(chromosomes)) > 1L) {
        stop("Supply chromosome anchors for individuals displayed on multiple chromosomes.", call. = FALSE)
      }
    }
    for (selector in list(chr, params$chr)) {
      if (length(selector) && !all(is.na(selector))) {
        d <- d[is.na(d$chr) | d$chr %in% selector, , drop = FALSE]
      }
    }
    if (length(window$start)) d <- d[d$x >= window$start, , drop = FALSE]
    if (length(window$end)) d <- d[d$x <= window$end, , drop = FALSE]
    if (length(params$subset)) {
      d <- d[d$x >= min(params$subset) & d$x <= max(params$subset), , drop = FALSE]
    }
    d$track <- rep(track, nrow(d))
    .alignseq_validate_chunks(d, params$mode, params$max_columns)
    d
  })
  out <- dplyr::bind_rows(rows[FALSE, , drop = FALSE], tables)
  # An entirely empty selection still needs track names for facet_genomics()
  # to create its panels. setup_data removes these layout-only rows.
  if (!nrow(out) && nrow(tracks)) {
    out <- out[rep(NA_integer_, nrow(tracks)), , drop = FALSE]
    out$track <- tracks$track
    out$individual <- tracks$individual
  }
  attr(out, "ggexon_panel_role") <- "annotation"
  out
}

# Evaluate the same feature and intron outlines used by the exon geom.
.alignseq_transcript_edge <- function(e, x, target) {
  if (x < min(e$xmin) || x > max(e$xmax))
    stop("The callout anchor lies outside displayed transcript `", target, "`.", call. = FALSE)
  covered <- e$xmin <= x & e$xmax >= x
  if (!"intron_shape" %in% names(e)) {
    return(if (any(covered)) max(e$ymax[covered]) else e$y_middle[1L])
  }
  if (any(covered)) {
    rect <- .exon2_trim_terminal_rects(e)
    heights <- rect$ymax[rect$xmin <= x & rect$xmax >= x]
    arrow <- .exon2_arrow_data(e)
    if (nrow(arrow) && x >= min(arrow$x) && x <= max(arrow$x)) {
      heights <- c(heights, stats::approx(arrow$x[1:2], arrow$y[1:2], x)$y)
    }
    return(max(heights))
  }
  paths <- .exon2_intron_data(e, shape = e$intron_shape[1L],
    direction = e$chevron_direction[1L], peak = e$intron_peak[1L])
  for (path in split(paths, paths$group)) {
    if (min(path$x) <= x && x <= max(path$x))
      return(stats::approx(path$x, path$y, x)$y)
  }
  stop("No displayed intron at the callout anchor for transcript `", target, "`.", call. = FALSE)
}

bind_alignseq_transcripts <- function(data, layers) {
  alignments <- which(vapply(layers, function(l)
    inherits(l$geom, "GeomAlignseq") && identical(l$geom_params$anchor, "transcript"), logical(1)))
  if (!length(alignments)) return(data)
  exons <- which(vapply(layers, function(l) inherits(l$geom, "GeomExon"), logical(1)))
  for (i in alignments) {
    d <- data[[i]]
    if (!nrow(d)) next
    # Position scales may censor bases before geom setup. Only displayed
    # bases contribute to the midpoint or require a transcript target.
    d <- d[is.finite(d$x) & is.finite(d$y), , drop = FALSE]
    if (!nrow(d)) {
      data[[i]] <- d
      next
    }
    params <- layers[[i]]$geom_params
    id <- as.character(d$individual %||% d$track)
    targets <- unname(params$transcripts[id])
    if (anyNA(targets)) stop("Supply transcripts for each displayed individual: ",
      paste(unique(id[is.na(targets)]), collapse = ", "), call. = FALSE)
    groups <- split(seq_len(nrow(d)), interaction(d$PANEL, id, d$sequence, drop = TRUE))
    d$xend <- d$yend <- NA_real_
    for (indices in groups) {
      j <- indices[1L]
      target <- targets[j]
      chromosomes <- unique(as.character(d$source_chr[indices]))
      chromosomes <- chromosomes[!is.na(chromosomes) & nzchar(chromosomes)]
      if (length(chromosomes) > 1L)
        stop("Ambiguous chromosomes for DNA sequence `", d$sequence[j], "`.", call. = FALSE)
      models <- lapply(data[exons], function(e) {
        identity <- e$transcript_id %||% e$transcripts
        visible <- is.finite(e$xmin) & is.finite(e$xmax) & is.finite(e$ymin) & is.finite(e$ymax)
        if (length(chromosomes) && !is.null(e$source_chr)) {
          visible <- visible & (is.na(e$source_chr) | e$source_chr == "" | e$source_chr == chromosomes)
        }
        e[which(visible & e$PANEL == d$PANEL[j] & e$track == d$track[j] & identity == target),
          , drop = FALSE]
      })
      matches <- which(vapply(models, nrow, integer(1)) > 0L)
      if (!length(matches)) stop("No displayed exon geometry for transcript `", target,
        "` on track `", d$track[j], "`", if (length(chromosomes)) paste0(" on chromosome `", chromosomes, "`"),
        "; add a matching exon layer.", call. = FALSE)
      if (length(matches) > 1L) stop("Ambiguous displayed exon layers for transcript `",
        target, "`; select one exon layer for this target.", call. = FALSE)
      e <- models[[matches]]
      position <- layers[[exons[matches]]]$position
      if (length(unique(e$y_middle)) != 1L || length(unique(e$transcripts)) != 1L)
        stop("Ambiguous displayed rows for transcript `", target, "`.", call. = FALSE)
      # Exon2 may compress this particular transcript locally. Interpolate
      # its stored source-to-display endpoints when there is no shared map.
      if (!"genomic_x" %in% names(d) && "genomic_xmin" %in% names(e) &&
          any(e$xmin != e$genomic_xmin | e$xmax != e$genomic_xmax)) {
        source <- c(e$genomic_xmin, e$genomic_xmax)
        display <- c(e$xmin, e$xmax)
        if (anyDuplicated(source) && any(vapply(split(display, source),
            function(v) length(unique(v)) > 1L, logical(1)))) {
          stop("Ambiguous local exon compression; use scale_x_ggexon_genomic().", call. = FALSE)
        }
        raw <- d$x[indices]
        d$x[indices] <- stats::approx(source, display, xout = raw, ties = mean,
          rule = 2)$y + pmin(raw - min(source), 0) + pmax(raw - max(source), 0)
      } else if (inherits(position, "PositionNudge")) {
        # With a shared map the bases are already in panel coordinates.
        # Apply the same display-space offset as the matching feature.
        nudge <- unique(position$x %||% e$nudge_x %||% 0)
        if (length(nudge) != 1L || !is.finite(nudge))
          stop("Transcript anchoring requires a constant horizontal position nudge.", call. = FALSE)
        d$x[indices] <- d$x[indices] + nudge
      }
      d$y[indices] <- max(e$ymax)
      chunks <- if (params$mode == "rows") list(indices) else split(indices, d$detail_id[indices])
      for (chunk in chunks) {
        x <- mean(range(d$x[chunk]))
        d$xend[chunk] <- x
        d$yend[chunk] <- .alignseq_transcript_edge(e, x, target)
      }
    }
    data[[i]] <- GeomAlignseq$setup_data(d, params)
  }
  data
}
