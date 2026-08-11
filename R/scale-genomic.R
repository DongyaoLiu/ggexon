#' Compress introns while keeping genomic x-axis labels
#'
#' `scale_x_ggexon_genomic()` adds a panel-specific x transform for exon-style
#' genomic tracks. Exonic intervals keep their original length, while the gaps
#' between exon-union intervals are compressed by `intron_factor`. Axis breaks
#' are drawn at compressed display positions but labelled with the original
#' genomic coordinates.
#'
#' This scale is intended for exon-structure layers such as [geom_exon()] and
#' [geom_exon2()]. It builds one transform per panel from the union of exon-like
#' intervals, so multiple transcripts in the same panel stay aligned.
#'
#' Use [guide_x_ggexon_piecewise()] in `guide` when the axis should display
#' representative exon and intron scale bars instead of ordinary genomic ticks.
#'
#' @param intron_factor Numeric compression factor for intronic gaps. For
#'   example, `10` draws a 10 kb intron as 1 kb while leaving exon widths
#'   unchanged.
#' @param species Optional character vector selecting the species, strains, ids,
#'   or tracks that should receive intron compression. When `NULL`, all eligible
#'   panels are compressed. Unselected panels stay on their original genomic
#'   coordinate scale.
#' @param match_by Panel-layout column used to match `species`. `"auto"` checks
#'   common layout columns such as `species`, `strain`, `id`, and `track`.
#' @param breaks Genomic-coordinate breaks. Use `waiver()` for pretty breaks
#'   over the original genomic range, `NULL` to hide breaks, a numeric vector,
#'   or a function that receives original genomic limits.
#' @param labels Break labels. Use `waiver()` for `scales::label_number()`,
#'   `NULL` for no labels, a character vector, or a function applied to original
#'   genomic break values.
#' @param minor_breaks Minor breaks in original genomic coordinates. Defaults to
#'   `NULL`.
#' @param n.breaks Approximate number of pretty major breaks when
#'   `breaks = waiver()`.
#' @param guide Axis guide. Use `waiver()` or `"genomic"` for ordinary
#'   genomic-coordinate ticks, `"none"` to hide transformed x-axis ticks, or
#'   [guide_x_ggexon_piecewise()] for representative exon/intron scale bars.
#'
#' @return A ggexon x-scale specification.
#' @export
scale_x_ggexon_genomic <- function(intron_factor = 10,
                                   species = NULL,
                                   match_by = c("auto", "species", "strain", "id", "track"),
                                   breaks = waiver(),
                                   labels = waiver(),
                                   minor_breaks = NULL,
                                   n.breaks = 5,
                                   guide = waiver()) {
  match_by <- match.arg(match_by)
  guide <- ggexon_genomic_x_normalize_guide(guide)
  if (!is.numeric(intron_factor) ||
      length(intron_factor) != 1L ||
      is.na(intron_factor) ||
      !is.finite(intron_factor) ||
      intron_factor <= 0) {
    stop("`intron_factor` must be one positive numeric value.", call. = FALSE)
  }
  if (!is.null(n.breaks) &&
      (!is.numeric(n.breaks) || length(n.breaks) != 1L || is.na(n.breaks) || n.breaks < 1)) {
    stop("`n.breaks` must be one positive numeric value or `NULL`.", call. = FALSE)
  }
  if (!is.null(species)) {
    if (!is.character(species) && !is.factor(species)) {
      stop("`species` must be a character vector or `NULL`.", call. = FALSE)
    }
    species <- unique(as.character(species))
    species <- species[!is.na(species) & nzchar(species)]
    if (length(species) == 0L) {
      species <- NULL
    }
  }

  structure(
    list(
      intron_factor = as.numeric(intron_factor),
      species = species,
      match_by = match_by,
      breaks = breaks,
      labels = labels,
      minor_breaks = minor_breaks,
      n.breaks = if (is.null(n.breaks)) NULL else as.integer(n.breaks),
      guide = guide
    ),
    class = "ggexon_genomic_x_scale_spec"
  )
}

#' Draw representative exon and intron scale bars for genomic x scaling
#'
#' `guide_x_ggexon_piecewise()` is used with [scale_x_ggexon_genomic()] to
#' replace ordinary x-axis ticks with representative first-exon and first-intron
#' scale bars. This is useful because intron-compressed tracks use different
#' display scales for exon and intron regions.
#'
#' @param by Grouping used to choose representative intervals. `"transcripts"`
#'   draws one first-exon/first-intron pair per transcript, `"track"` draws one
#'   per track, and `"panel"` draws one per panel.
#' @param representative Representative interval selection. Currently only
#'   `"first"` is supported.
#' @param position Axis position. Currently only `"bottom"` is supported.
#' @param label Logical; draw text labels for representative intervals.
#' @param show_exon,show_intron Logical; include representative exon and/or
#'   intron scale bars.
#'
#' @return A ggexon genomic x-axis guide specification.
#' @export
guide_x_ggexon_piecewise <- function(by = c("transcripts", "track", "panel"),
                                     representative = c("first"),
                                     position = c("bottom"),
                                     label = TRUE,
                                     show_exon = TRUE,
                                     show_intron = TRUE) {
  by <- match.arg(by)
  representative <- match.arg(representative)
  position <- match.arg(position)
  if (!is.logical(label) || length(label) != 1L || is.na(label)) {
    stop("`label` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.logical(show_exon) || length(show_exon) != 1L || is.na(show_exon)) {
    stop("`show_exon` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.logical(show_intron) || length(show_intron) != 1L || is.na(show_intron)) {
    stop("`show_intron` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!isTRUE(show_exon) && !isTRUE(show_intron)) {
    stop("At least one of `show_exon` or `show_intron` must be `TRUE`.", call. = FALSE)
  }

  structure(
    list(
      type = "piecewise",
      by = by,
      representative = representative,
      position = position,
      label = label,
      show_exon = show_exon,
      show_intron = show_intron
    ),
    class = c("ggexon_genomic_x_piecewise_guide", "ggexon_genomic_x_guide")
  )
}

ggexon_genomic_x_normalize_guide <- function(guide) {
  if (is.waive(guide) || identical(guide, "genomic")) {
    return(structure(
      list(type = "genomic"),
      class = c("ggexon_genomic_x_genomic_guide", "ggexon_genomic_x_guide")
    ))
  }
  if (is.null(guide) || identical(guide, "none")) {
    return(structure(
      list(type = "none"),
      class = c("ggexon_genomic_x_none_guide", "ggexon_genomic_x_guide")
    ))
  }
  if (inherits(guide, "ggexon_genomic_x_guide")) {
    return(guide)
  }
  stop(
    "`guide` must be `waiver()`, \"genomic\", \"none\", or `guide_x_ggexon_piecewise()`.",
    call. = FALSE
  )
}

ggexon_genomic_x_guide_type <- function(scale_spec) {
  scale_spec$guide$type %||% "genomic"
}

.abort_unsupported_coverage_x_transform <- function(layout, transform) {
  layout_df <- layout$layout %||% NULL
  if (!is.data.frame(layout_df) || nrow(layout_df) == 0L) {
    return(invisible(FALSE))
  }
  roles <- link_panel_type(layout_df)
  if (!any(roles == "coverage", na.rm = TRUE)) {
    return(invisible(FALSE))
  }
  cli::cli_abort(c(
    "First-class coverage panels contain continuous BigWig signal and are not supported with {transform}.",
    "i" = "Use the ordinary genomic x scale for plots containing coverage panels."
  ))
}

#' @export
ggplot_add.ggexon_genomic_x_scale_spec <- function(object, plot, ...) {
  if (!is_ggexon(plot)) {
    stop("`scale_x_ggexon_genomic()` can only be added to a ggexon plot.", call. = FALSE)
  }
  plot@genomic_x_scale <- object
  plot
}

apply_ggexon_genomic_x_scale <- function(data, scale_spec, layout = NULL) {
  if (is.null(scale_spec)) {
    return(list(data = data, transforms = NULL))
  }
  .abort_unsupported_coverage_x_transform(
    layout,
    "exon/intron-compressed genomic x scales"
  )

  source_rows <- lapply(data, ggexon_genomic_x_source_rows)
  source_rows <- source_rows[lengths(source_rows) > 0L]
  if (length(source_rows) == 0L) {
    return(list(data = data, transforms = NULL))
  }

  source_df <- do.call(rbind, source_rows)
  panel_ids <- sort(unique(source_df$PANEL))
  selected_panel_ids <- ggexon_genomic_x_selected_panels(
    scale_spec = scale_spec,
    layout = layout,
    source_panel_ids = panel_ids
  )
  partial_selection <- !is.null(selected_panel_ids) &&
    length(setdiff(panel_ids, selected_panel_ids)) > 0L
  if (isTRUE(partial_selection) && !ggexon_genomic_x_has_free_x(layout)) {
    warning(
      "`scale_x_ggexon_genomic(species = ...)` is being used with a fixed x scale. ",
      "Use `facet_genomics(scales = \"free_x\")` when transformed and untransformed ",
      "panels should keep independent coordinate systems.",
      call. = FALSE
    )
  }

  transforms <- vector("list", length(panel_ids))
  names(transforms) <- as.character(panel_ids)
  for (i in seq_along(panel_ids)) {
    panel_id <- panel_ids[[i]]
    panel_intron_factor <- if (is.null(selected_panel_ids) || panel_id %in% selected_panel_ids) {
      scale_spec$intron_factor
    } else {
      1
    }
    transforms[[i]] <- build_ggexon_genomic_x_transform(
      source_df[source_df$PANEL == panel_id, , drop = FALSE],
      intron_factor = panel_intron_factor
    )
  }
  transforms <- Filter(Negate(is.null), transforms)
  if (length(transforms) == 0L) {
    return(list(data = data, transforms = NULL, axis_data = NULL))
  }

  axis_data <- ggexon_genomic_x_piecewise_axis_data(
    source_df = source_df,
    transforms = transforms,
    scale_spec = scale_spec
  )
  data <- lapply(data, transform_ggexon_genomic_x_layer, transforms = transforms)
  list(data = data, transforms = transforms, axis_data = axis_data)
}

ggexon_genomic_x_selected_panels <- function(scale_spec,
                                             layout = NULL,
                                             source_panel_ids = NULL) {
  selected <- scale_spec$species
  if (is.null(selected)) {
    return(NULL)
  }
  if (is.null(layout) || is.null(layout$layout)) {
    stop(
      "`scale_x_ggexon_genomic(species = ...)` needs a resolved facet layout.",
      call. = FALSE
    )
  }

  layout_df <- as.data.frame(layout$layout)
  if (!"PANEL" %in% names(layout_df)) {
    stop(
      "`scale_x_ggexon_genomic(species = ...)` could not find `PANEL` in the resolved layout.",
      call. = FALSE
    )
  }

  match_columns <- ggexon_genomic_x_match_columns(layout_df, scale_spec$match_by)
  available <- unique(unlist(lapply(match_columns, function(col) {
    as.character(layout_df[[col]])
  }), use.names = FALSE))
  available <- available[!is.na(available) & nzchar(available)]
  unmatched <- setdiff(selected, available)
  if (length(unmatched) == length(selected)) {
    stop(
      "`species` did not match any panel layout values. Available values: ",
      paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(unmatched) > 0L) {
    warning(
      "`species` values not found in the panel layout: ",
      paste(unmatched, collapse = ", "),
      call. = FALSE
    )
  }

  keep <- rep(FALSE, nrow(layout_df))
  for (col in match_columns) {
    keep <- keep | as.character(layout_df[[col]]) %in% selected
  }
  selected_panel_ids <- ggexon_panel_id(layout_df$PANEL[keep])
  selected_panel_ids <- selected_panel_ids[!is.na(selected_panel_ids)]
  selected_panel_ids <- unique(selected_panel_ids)
  if (!is.null(source_panel_ids)) {
    selected_panel_ids <- intersect(selected_panel_ids, source_panel_ids)
  }
  if (length(selected_panel_ids) == 0L) {
    stop(
      "`species` matched the layout, but none of the matched panels contain exon-style x intervals.",
      call. = FALSE
    )
  }
  selected_panel_ids
}

ggexon_genomic_x_match_columns <- function(layout_df, match_by = "auto") {
  candidates <- switch(
    match_by,
    auto = c("species", "strain", "strains", "id", "individual", "track"),
    strain = c("strain", "strains"),
    species = "species",
    id = "id",
    track = "track"
  )
  match_columns <- intersect(candidates, names(layout_df))
  if (length(match_columns) == 0L) {
    stop(
      "`match_by = \"", match_by, "\"` did not find a matching column in the panel layout.",
      call. = FALSE
    )
  }
  match_columns
}

ggexon_genomic_x_has_free_x <- function(layout = NULL) {
  if (is.null(layout) ||
      is.null(layout$facet) ||
      is.null(layout$facet$params) ||
      is.null(layout$facet$params$free) ||
      is.null(layout$facet$params$free$x)) {
    return(TRUE)
  }
  isTRUE(layout$facet$params$free$x)
}

ggexon_genomic_x_source_rows <- function(layer_data) {
  if (!is.data.frame(layer_data) ||
      !all(c("PANEL", "xmin", "xmax") %in% names(layer_data)) ||
      any(c("tspecies", "qspecies") %in% names(layer_data))) {
    return(data.frame())
  }
  if (!any(c("transcripts", "type", "strand") %in% names(layer_data))) {
    return(data.frame())
  }

  xmin <- layer_data$genomic_xmin %||% layer_data$xmin
  xmax <- layer_data$genomic_xmax %||% layer_data$xmax
  out <- data.frame(
    PANEL = ggexon_panel_id(layer_data$PANEL),
    track = if ("track" %in% names(layer_data)) as.character(layer_data$track) else NA_character_,
    transcripts = if ("transcripts" %in% names(layer_data)) as.character(layer_data$transcripts) else NA_character_,
    type = if ("type" %in% names(layer_data)) as.character(layer_data$type) else NA_character_,
    genomic_start = pmin(as.numeric(xmin), as.numeric(xmax)),
    genomic_end = pmax(as.numeric(xmin), as.numeric(xmax)),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$genomic_start) & is.finite(out$genomic_end) & out$genomic_end > out$genomic_start, , drop = FALSE]
  rownames(out) <- NULL
  out
}

ggexon_genomic_x_piecewise_axis_data <- function(source_df, transforms, scale_spec) {
  guide <- scale_spec$guide
  if (!identical(ggexon_genomic_x_guide_type(scale_spec), "piecewise") ||
      is.null(source_df) ||
      nrow(source_df) == 0L ||
      is.null(transforms) ||
      length(transforms) == 0L) {
    return(NULL)
  }

  group_cols <- switch(
    guide$by,
    transcripts = c("PANEL", "track", "transcripts"),
    track = c("PANEL", "track"),
    panel = "PANEL"
  )
  group_cols <- intersect(group_cols, names(source_df))
  source_df$axis_group <- interaction(source_df[, group_cols, drop = FALSE], drop = TRUE, sep = " / ")

  pieces <- lapply(split(source_df, source_df$axis_group), function(group_df) {
    panel_id <- unique(group_df$PANEL)
    panel_id <- panel_id[!is.na(panel_id)][[1L]]
    transform <- transforms[[as.character(panel_id)]]
    if (is.null(transform)) {
      return(NULL)
    }

    intervals <- reduce_ggexon_genomic_intervals(group_df$genomic_start, group_df$genomic_end)
    if (nrow(intervals) == 0L) {
      return(NULL)
    }

    out <- list()
    if (isTRUE(guide$show_exon)) {
      out[[length(out) + 1L]] <- ggexon_genomic_x_axis_interval(
        group_df = group_df,
        interval = intervals[1L, , drop = FALSE],
        transform = transform,
        region_type = "exon",
        label = isTRUE(guide$label)
      )
    }

    if (isTRUE(guide$show_intron) &&
        nrow(intervals) >= 2L &&
        intervals$start[[2L]] > intervals$end[[1L]]) {
      out[[length(out) + 1L]] <- ggexon_genomic_x_axis_interval(
        group_df = group_df,
        interval = data.frame(start = intervals$end[[1L]], end = intervals$start[[2L]]),
        transform = transform,
        region_type = "intron",
        label = isTRUE(guide$label)
      )
    }
    if (length(out) == 0L) {
      return(NULL)
    }
    do.call(rbind, out)
  })

  axis_data <- do.call(rbind, Filter(Negate(is.null), pieces))
  if (is.null(axis_data) || nrow(axis_data) == 0L) {
    return(NULL)
  }
  axis_data$axis_group <- as.character(axis_data$axis_group)
  axis_data$axis_group_index <- NA_integer_
  axis_data$axis_group_count <- NA_integer_
  for (panel_id in unique(axis_data$PANEL)) {
    idx <- which(axis_data$PANEL == panel_id)
    panel_groups <- unique(axis_data$axis_group[idx])
    axis_data$axis_group_index[idx] <- match(axis_data$axis_group[idx], panel_groups)
    axis_data$axis_group_count[idx] <- length(panel_groups)
  }
  rownames(axis_data) <- NULL
  axis_data
}

ggexon_genomic_x_axis_interval <- function(group_df,
                                           interval,
                                           transform,
                                           region_type,
                                           label = TRUE) {
  genomic_start <- interval$start[[1L]]
  genomic_end <- interval$end[[1L]]
  plot_start <- ggexon_genomic_to_plot_x(genomic_start, transform)
  plot_end <- ggexon_genomic_to_plot_x(genomic_end, transform)
  genomic_width <- genomic_end - genomic_start
  plot_width <- plot_end - plot_start
  data.frame(
    PANEL = group_df$PANEL[[1L]],
    track = group_df$track[[1L]] %||% NA_character_,
    transcripts = group_df$transcripts[[1L]] %||% NA_character_,
    axis_group = group_df$axis_group[[1L]],
    region_type = region_type,
    genomic_start = genomic_start,
    genomic_end = genomic_end,
    plot_start = plot_start,
    plot_end = plot_end,
    genomic_width = genomic_width,
    plot_width = plot_width,
    label = if (isTRUE(label)) ggexon_genomic_x_axis_label(region_type, genomic_width, plot_width) else "",
    stringsAsFactors = FALSE
  )
}

ggexon_genomic_x_axis_label <- function(region_type, genomic_width, plot_width) {
  if (identical(region_type, "intron") && is.finite(plot_width) && plot_width > 0) {
    factor <- genomic_width / plot_width
    if (is.finite(factor) && factor > 1.01) {
      return(paste0("intron ", ggexon_format_bp(genomic_width), " /", signif(factor, 3)))
    }
  }
  paste(region_type, ggexon_format_bp(genomic_width))
}

ggexon_format_bp <- function(x) {
  if (!is.finite(x)) {
    return(NA_character_)
  }
  x <- abs(x)
  if (x >= 1e6) {
    return(paste0(format(round(x / 1e6, 2), trim = TRUE), " Mb"))
  }
  if (x >= 1e3) {
    return(paste0(format(round(x / 1e3, 2), trim = TRUE), " kb"))
  }
  paste0(format(round(x, 0), trim = TRUE), " bp")
}

build_ggexon_genomic_x_transform <- function(source_df, intron_factor = 10) {
  intervals <- reduce_ggexon_genomic_intervals(source_df$genomic_start, source_df$genomic_end)
  if (nrow(intervals) == 0L) {
    return(NULL)
  }

  pieces <- list()
  current_plot <- intervals$start[[1L]]
  for (i in seq_len(nrow(intervals))) {
    if (i > 1L) {
      gap_start <- intervals$end[[i - 1L]]
      gap_end <- intervals$start[[i]]
      if (gap_end > gap_start) {
        gap_width <- gap_end - gap_start
        pieces[[length(pieces) + 1L]] <- data.frame(
          genomic_start = gap_start,
          genomic_end = gap_end,
          plot_start = current_plot,
          plot_end = current_plot + gap_width / intron_factor,
          slope = 1 / intron_factor,
          region_type = "intron",
          stringsAsFactors = FALSE
        )
        current_plot <- pieces[[length(pieces)]]$plot_end
      }
    }

    exon_width <- intervals$end[[i]] - intervals$start[[i]]
    pieces[[length(pieces) + 1L]] <- data.frame(
      genomic_start = intervals$start[[i]],
      genomic_end = intervals$end[[i]],
      plot_start = current_plot,
      plot_end = current_plot + exon_width,
      slope = 1,
      region_type = "exon",
      stringsAsFactors = FALSE
    )
    current_plot <- pieces[[length(pieces)]]$plot_end
  }

  transform <- do.call(rbind, pieces)
  transform$PANEL <- unique(source_df$PANEL)[[1L]]
  rownames(transform) <- NULL
  transform
}

reduce_ggexon_genomic_intervals <- function(start, end) {
  intervals <- data.frame(start = as.numeric(start), end = as.numeric(end))
  intervals <- intervals[is.finite(intervals$start) & is.finite(intervals$end) & intervals$end > intervals$start, , drop = FALSE]
  if (nrow(intervals) == 0L) {
    return(data.frame(start = numeric(), end = numeric()))
  }
  intervals <- intervals[order(intervals$start, intervals$end), , drop = FALSE]

  out <- list()
  current_start <- intervals$start[[1L]]
  current_end <- intervals$end[[1L]]
  for (i in seq_len(nrow(intervals))[-1L]) {
    next_start <- intervals$start[[i]]
    next_end <- intervals$end[[i]]
    if (next_start <= current_end) {
      current_end <- max(current_end, next_end)
    } else {
      out[[length(out) + 1L]] <- data.frame(start = current_start, end = current_end)
      current_start <- next_start
      current_end <- next_end
    }
  }
  out[[length(out) + 1L]] <- data.frame(start = current_start, end = current_end)
  do.call(rbind, out)
}

transform_ggexon_genomic_x_layer <- function(layer_data, transforms) {
  if (!is.data.frame(layer_data) || !"PANEL" %in% names(layer_data)) {
    return(layer_data)
  }

  x_columns <- intersect(c("x", "xmin", "xmax", "xend", "xintercept"), names(layer_data))
  if (length(x_columns) == 0L) {
    return(layer_data)
  }

  panel_ids <- ggexon_panel_id(layer_data$PANEL)
  for (panel_id in unique(panel_ids)) {
    transform <- transforms[[as.character(panel_id)]]
    if (is.null(transform)) {
      next
    }
    idx <- which(panel_ids == panel_id)
    for (col in x_columns) {
      genomic_col <- paste0("genomic_", col)
      if (!genomic_col %in% names(layer_data)) {
        layer_data[[genomic_col]] <- layer_data[[col]]
      }
      layer_data[[col]][idx] <- ggexon_genomic_to_plot_x(layer_data[[genomic_col]][idx], transform)
    }
  }

  layer_data
}

ggexon_genomic_to_plot_x <- function(x, transform) {
  vapply(as.numeric(x), function(value) {
    if (!is.finite(value) || is.null(transform) || nrow(transform) == 0L) {
      return(value)
    }
    first <- transform[1L, , drop = FALSE]
    last <- transform[nrow(transform), , drop = FALSE]
    if (value < first$genomic_start[[1L]]) {
      return(first$plot_start[[1L]] - (first$genomic_start[[1L]] - value))
    }
    if (value > last$genomic_end[[1L]]) {
      return(last$plot_end[[1L]] + (value - last$genomic_end[[1L]]))
    }
    idx <- which(value >= transform$genomic_start & value <= transform$genomic_end)
    idx <- idx[[length(idx)]]
    row <- transform[idx, , drop = FALSE]
    row$plot_start[[1L]] + (value - row$genomic_start[[1L]]) * row$slope[[1L]]
  }, numeric(1))
}

ggexon_plot_to_genomic_x <- function(x, transform) {
  vapply(as.numeric(x), function(value) {
    if (!is.finite(value) || is.null(transform) || nrow(transform) == 0L) {
      return(value)
    }
    first <- transform[1L, , drop = FALSE]
    last <- transform[nrow(transform), , drop = FALSE]
    if (value < first$plot_start[[1L]]) {
      return(first$genomic_start[[1L]] - (first$plot_start[[1L]] - value))
    }
    if (value > last$plot_end[[1L]]) {
      return(last$genomic_end[[1L]] + (value - last$plot_end[[1L]]))
    }
    idx <- which(value >= transform$plot_start & value <= transform$plot_end)
    idx <- idx[[length(idx)]]
    row <- transform[idx, , drop = FALSE]
    row$genomic_start[[1L]] + (value - row$plot_start[[1L]]) / row$slope[[1L]]
  }, numeric(1))
}

apply_ggexon_genomic_x_axis <- function(layout, scale_spec) {
  transforms <- layout$genomic_x_transforms
  if (is.null(scale_spec) || is.null(transforms) || length(transforms) == 0L) {
    return(layout)
  }
  if (is.null(layout$panel_params) || length(layout$panel_params) == 0L) {
    return(layout)
  }

  layout_df <- as.data.frame(layout$layout)
  for (i in seq_len(nrow(layout_df))) {
    panel_id <- as.integer(layout_df$PANEL[[i]])
    transform <- transforms[[as.character(panel_id)]]
    if (is.null(transform) || panel_id > length(layout$panel_params)) {
      next
    }

    x_view <- layout$panel_params[[panel_id]]$x
    if (is.null(x_view)) {
      next
    }
    guide_type <- ggexon_genomic_x_guide_type(scale_spec)
    if (identical(guide_type, "none") || identical(guide_type, "piecewise")) {
      x_view$scale <- x_view$scale$clone()
      x_view$breaks <- numeric()
      x_view$minor_breaks <- numeric()
      x_view$scale$breaks <- numeric()
      x_view$scale$labels <- character()
      x_view$scale$minor_breaks <- NULL
      layout$panel_params[[panel_id]]$x <- x_view
      next
    }

    genomic_limits <- range(c(transform$genomic_start, transform$genomic_end), finite = TRUE)
    genomic_breaks <- ggexon_genomic_scale_breaks(scale_spec$breaks, genomic_limits, scale_spec$n.breaks)
    genomic_breaks <- genomic_breaks[is.finite(genomic_breaks)]
    display_breaks <- ggexon_genomic_to_plot_x(genomic_breaks, transform)
    labels <- ggexon_genomic_scale_labels(scale_spec$labels, genomic_breaks)

    x_view$scale <- x_view$scale$clone()
    x_view$breaks <- display_breaks
    x_view$scale$breaks <- display_breaks
    x_view$scale$labels <- labels

    minor_breaks <- ggexon_genomic_scale_breaks(scale_spec$minor_breaks, genomic_limits, scale_spec$n.breaks)
    if (length(minor_breaks) > 0L) {
      minor_breaks <- minor_breaks[is.finite(minor_breaks)]
      x_view$minor_breaks <- ggexon_genomic_to_plot_x(minor_breaks, transform)
      x_view$scale$minor_breaks <- x_view$minor_breaks
    } else {
      x_view$minor_breaks <- numeric()
      x_view$scale$minor_breaks <- NULL
    }

    layout$panel_params[[panel_id]]$x <- x_view
  }

  layout
}

ggexon_genomic_scale_breaks <- function(breaks, genomic_limits, n_breaks = 5) {
  if (is.null(breaks)) {
    return(numeric())
  }
  if (is.waive(breaks)) {
    return(pretty(genomic_limits, n = n_breaks %||% 5L))
  }
  if (is.function(breaks)) {
    return(as.numeric(breaks(genomic_limits)))
  }
  as.numeric(breaks)
}

ggexon_genomic_scale_labels <- function(labels, genomic_breaks) {
  if (is.null(labels)) {
    return(rep("", length(genomic_breaks)))
  }
  if (is.waive(labels)) {
    return(scales::label_number()(genomic_breaks))
  }
  if (is.function(labels)) {
    return(as.character(labels(genomic_breaks)))
  }
  labels <- as.character(labels)
  if (length(labels) != length(genomic_breaks)) {
    stop("`labels` must have the same length as resolved genomic breaks.", call. = FALSE)
  }
  labels
}

ggexon_panel_id <- function(panel) {
  if (is.factor(panel)) {
    return(as.integer(as.character(panel)))
  }
  as.integer(panel)
}
