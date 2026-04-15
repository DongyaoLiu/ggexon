#' Cross-panel annotation specification
#'
#' Create a lightweight specification for annotations that will be resolved
#' after plot build and injected during gtable rendering.
#'
#' @param data A data frame containing anchor information.
#' @param from,to Aesthetic mappings describing the source and target anchors.
#'   Both mappings must include `panel`, `x`, and `y`.
#' @param ...,colour,color,linewidth,linetype,alpha Optional styling
#'   parameters stored on the specification.
#' @param geom Annotation geometry. V1 only supports `"line"`.
#'
#' @return An object of class `"cross_panel_annotation"`.
#' @export
annotate_cross_panel <- function(data,
                                 from,
                                 to,
                                 ...,
                                 colour = NULL,
                                 color = NULL,
                                 linewidth = NULL,
                                 linetype = NULL,
                                 alpha = NULL,
                                 geom = "line") {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  from <- validate_cross_panel_mapping(from, "from")
  to <- validate_cross_panel_mapping(to, "to")
  params <- compact_cross_panel_params(list(
    colour = colour %||% color,
    linewidth = linewidth,
    linetype = linetype,
    alpha = alpha,
    ...
  ))

  new_cross_panel_annotation(
    data = data,
    mode = "paired",
    from = from,
    to = to,
    geom = geom,
    params = params
  )
}

#' Link shared ids across panels
#'
#' Build a cross-panel annotation spec from one row per anchor. The resulting
#' specification is resolved after plot build and drawn during rendering.
#'
#' @param data A data frame containing one row per anchor.
#' @param id,panel,x,y Column names identifying the anchor id, panel, and
#'   position.
#' @param ...,colour,color,linewidth,linetype,alpha Optional styling
#'   parameters stored on the specification.
#'
#' @return An object of class `"cross_panel_annotation"`.
#' @export
link_panels <- function(data,
                        id,
                        panel,
                        x,
                        y,
                        ...,
                        colour = NULL,
                        color = NULL,
                        linewidth = NULL,
                        linetype = NULL,
                        alpha = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  required_cols <- c(id, panel, x, y)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "Missing required columns in {.arg data}: {.val {missing_cols}}."
    )
  }

  params <- compact_cross_panel_params(list(
    colour = colour %||% color,
    linewidth = linewidth,
    linetype = linetype,
    alpha = alpha,
    ...
  ))

  new_cross_panel_annotation(
    data = data,
    mode = "shared_id",
    columns = list(id = id, panel = panel, x = x, y = y),
    geom = "line",
    params = params
  )
}

new_cross_panel_annotation <- function(data,
                                       mode,
                                       from = NULL,
                                       to = NULL,
                                       columns = NULL,
                                       geom = "line",
                                       params = list()) {
  allowed_modes <- c("paired", "shared_id")
  if (!mode %in% allowed_modes) {
    cli::cli_abort("{.arg mode} must be one of {.val {allowed_modes}}.")
  }
  if (!identical(geom, "line")) {
    cli::cli_abort("V1 only supports {.val line} cross-panel annotations.")
  }

  structure(
    list(
      data = data,
      mode = mode,
      from = from,
      to = to,
      columns = columns,
      geom = geom,
      params = params
    ),
    class = "cross_panel_annotation"
  )
}

validate_cross_panel_mapping <- function(mapping, arg) {
  if (!ggplot2::is_mapping(mapping)) {
    cli::cli_abort("{.arg {arg}} must be created with {.fn aes}.")
  }

  mapping_names <- names(mapping)
  required_names <- c("panel", "x", "y")
  missing_names <- setdiff(required_names, mapping_names)
  if (length(missing_names) > 0L) {
    cli::cli_abort(
      "{.arg {arg}} must include mappings for {.val {required_names}}."
    )
  }

  mapping
}

compact_cross_panel_params <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' @export
ggplot_add.cross_panel_annotation <- function(object, plot, object_name) {
  if (!is_ggexon(plot)) {
    cli::cli_abort(
      "Cross-panel annotations currently support {.cls ggexon} plots only."
    )
  }

  plot@cross_panel_annotations <- c(plot@cross_panel_annotations, list(object))
  plot
}

build_cross_panel_annotations <- function(build, table) {
  annotations <- build@plot@cross_panel_annotations
  if (length(annotations) == 0L) {
    return(list())
  }

  panel_info <- collect_cross_panel_panel_info(build, table)

  lapply(annotations, function(annotation) {
    anchors <- resolve_cross_panel_anchors(build, annotation)
    anchors <- map_cross_panel_anchor_panels(build, anchors)
    list(
      annotation = annotation,
      anchors = anchors,
      panel_info = panel_info
    )
  })
}

inject_cross_panel_annotations <- function(table, build) {
  specs <- build_cross_panel_annotations(build, table)
  if (length(specs) == 0L) {
    return(table)
  }

  grob <- make_cross_panel_grob(specs, table, build)
  if (!is.null(grob)) {
    panel_idx <- grep("^panel", table$layout$name)
    table <- gtable::gtable_add_grob(
      table,
      grobs = grob,
      t = min(table$layout$t[panel_idx]),
      l = min(table$layout$l[panel_idx]),
      b = max(table$layout$b[panel_idx]),
      r = max(table$layout$r[panel_idx]),
      z = Inf,
      name = "cross-panel-annotations",
      clip = "off"
    )
  }

  attr(table, "cross_panel_annotations") <- specs
  table
}

collect_cross_panel_panel_info <- function(build, table) {
  panel_idx <- grep("^panel", table$layout$name)
  if (length(panel_idx) == 0L) {
    return(data.frame())
  }

  panel_layout <- build@layout$layout
  facet_cols <- setdiff(
    names(panel_layout),
    c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD")
  )

  info <- data.frame(
    PANEL = seq_along(panel_idx),
    table_t = table$layout$t[panel_idx],
    table_l = table$layout$l[panel_idx],
    table_b = table$layout$b[panel_idx],
    table_r = table$layout$r[panel_idx],
    stringsAsFactors = FALSE
  )

  if (length(facet_cols) > 0L) {
    facet_df <- panel_layout[, c("PANEL", facet_cols), drop = FALSE]
    facet_df$PANEL <- as.integer(facet_df$PANEL)
    info <- dplyr::left_join(info, facet_df, by = "PANEL")
  }

  info
}

resolve_cross_panel_anchors <- function(build, annotation) {
  switch(
    annotation$mode,
    paired = resolve_paired_cross_panel_anchors(annotation),
    shared_id = resolve_shared_id_cross_panel_anchors(annotation),
    cli::cli_abort("Unsupported cross-panel annotation mode: {.val {annotation$mode}}.")
  )
}

resolve_paired_cross_panel_anchors <- function(annotation) {
  from_df <- eval_cross_panel_mapping(annotation$data, annotation$from, "from")
  to_df <- eval_cross_panel_mapping(annotation$data, annotation$to, "to")

  cbind(
    from_df,
    stats::setNames(to_df, paste0("to_", names(to_df))),
    stringsAsFactors = FALSE
  )
}

resolve_shared_id_cross_panel_anchors <- function(annotation) {
  cols <- annotation$columns
  data <- annotation$data[, c(cols$id, cols$panel, cols$x, cols$y), drop = FALSE]
  names(data) <- c("id", "panel", "x", "y")

  split_data <- split(data, data$id, drop = TRUE)
  pairs <- lapply(split_data, function(df) {
    if (nrow(df) < 2L) {
      return(NULL)
    }
    df <- df[seq_len(2L), , drop = FALSE]
    data.frame(
      id = df$id[1],
      panel = df$panel[1],
      x = df$x[1],
      y = df$y[1],
      to_panel = df$panel[2],
      to_x = df$x[2],
      to_y = df$y[2],
      stringsAsFactors = FALSE
    )
  })

  pairs <- Filter(Negate(is.null), pairs)
  if (length(pairs) == 0L) {
    return(data.frame())
  }

  dplyr::bind_rows(pairs)
}

eval_cross_panel_mapping <- function(data, mapping, prefix = NULL) {
  out <- lapply(mapping[c("panel", "x", "y")], rlang::eval_tidy, data = data)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- c("panel", "x", "y")
  if (!is.null(prefix)) {
    out$id <- seq_len(nrow(out))
    out <- out[, c("id", "panel", "x", "y"), drop = FALSE]
  }
  out
}

map_cross_panel_anchor_panels <- function(build, anchors) {
  if (nrow(anchors) == 0L) {
    return(anchors)
  }

  anchors$PANEL <- resolve_cross_panel_panel_ids(build, anchors$panel)
  anchors$to_PANEL <- resolve_cross_panel_panel_ids(build, anchors$to_panel)
  anchors
}

resolve_cross_panel_panel_ids <- function(build, values) {
  if (length(values) == 0L) {
    return(integer())
  }

  panel_layout <- build@layout$layout
  panel_ids <- rep(NA_integer_, length(values))

  suppressWarnings({
    numeric_values <- as.integer(as.character(values))
  })
  numeric_match <- !is.na(numeric_values) & numeric_values %in% as.integer(panel_layout$PANEL)
  panel_ids[numeric_match] <- numeric_values[numeric_match]

  if (all(!is.na(panel_ids))) {
    return(panel_ids)
  }

  facet_cols <- setdiff(
    names(panel_layout),
    c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD")
  )
  if (length(facet_cols) == 0L) {
    return(panel_ids)
  }

  unresolved <- which(is.na(panel_ids))
  for (i in unresolved) {
    match_idx <- which(vapply(
      facet_cols,
      function(col) any(as.character(panel_layout[[col]]) == as.character(values[[i]])),
      logical(1)
    ))
    if (length(match_idx) == 0L) {
      next
    }
    col <- facet_cols[[match_idx[[1]]]]
    row_idx <- which(as.character(panel_layout[[col]]) == as.character(values[[i]]))
    if (length(row_idx) == 1L) {
      panel_ids[[i]] <- as.integer(panel_layout$PANEL[[row_idx]])
    }
  }

  panel_ids
}

make_cross_panel_grob <- function(specs, table, build) {
  panel_idx <- grep("^panel", table$layout$name)
  if (length(panel_idx) == 0L) {
    return(NULL)
  }

  panel_rows <- sort(unique(table$layout$t[panel_idx]))
  panel_cols <- sort(unique(table$layout$l[panel_idx]))
  span_t <- min(panel_rows)
  span_l <- min(panel_cols)
  span_b <- max(table$layout$b[panel_idx])
  span_r <- max(table$layout$r[panel_idx])
  segment_data <- prepare_cross_panel_segments(
    specs = specs,
    table = table,
    build = build,
    span_t = span_t,
    span_l = span_l
  )
  if (nrow(segment_data) == 0L) {
    return(NULL)
  }

  cross_panel_segments_grob(
    data = segment_data,
    widths = table$widths[span_l:span_r],
    heights = table$heights[span_t:span_b]
  )
}

prepare_cross_panel_segments <- function(specs, table, build, span_t, span_l) {
  rows <- lapply(specs, function(spec) {
    anchors <- spec$anchors
    if (nrow(anchors) == 0L) {
      return(NULL)
    }

    out <- lapply(seq_len(nrow(anchors)), function(i) {
      anchor_row <- anchors[i, , drop = FALSE]
      if (anyNA(anchor_row[, c("PANEL", "to_PANEL", "x", "y", "to_x", "to_y")])) {
        return(NULL)
      }

      from <- anchor_to_panel_relative(
        panel_id = anchor_row$PANEL[[1]],
        x = anchor_row$x[[1]],
        y = anchor_row$y[[1]],
        table = table,
        build = build,
        span_t = span_t,
        span_l = span_l
      )
      to <- anchor_to_panel_relative(
        panel_id = anchor_row$to_PANEL[[1]],
        x = anchor_row$to_x[[1]],
        y = anchor_row$to_y[[1]],
        table = table,
        build = build,
        span_t = span_t,
        span_l = span_l
      )

      if (is.null(from) || is.null(to)) {
        return(NULL)
      }

      data.frame(
        from_col = from$panel_col,
        from_row = from$panel_row,
        from_rel_x = from$rel_x,
        from_rel_y = from$rel_y,
        to_col = to$panel_col,
        to_row = to$panel_row,
        to_rel_x = to$rel_x,
        to_rel_y = to$rel_y,
        colour = spec$annotation$params$colour %||% "red",
        linewidth = spec$annotation$params$linewidth %||% 2,
        linetype = spec$annotation$params$linetype %||% 1,
        alpha = spec$annotation$params$alpha %||% NA_real_,
        stringsAsFactors = FALSE
      )
    })

    out <- Filter(Negate(is.null), out)
    if (length(out) == 0L) {
      return(NULL)
    }
    dplyr::bind_rows(out)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(data.frame())
  }
  dplyr::bind_rows(rows)
}

anchor_to_panel_relative <- function(panel_id, x, y, table, build, span_t, span_l) {
  panel_layout_row <- which(as.integer(build@layout$layout$PANEL) == as.integer(panel_id))
  if (length(panel_layout_row) != 1L) {
    return(NULL)
  }

  panel_params <- build@layout$panel_params[[panel_id]]
  x_range <- panel_params$x$continuous_range %||% panel_params$x.range
  y_range <- panel_params$y$continuous_range %||% panel_params$y.range
  if (is.null(x_range) || is.null(y_range)) {
    return(NULL)
  }

  panel_gtable_row <- grep(sprintf("^panel-%s-", panel_layout_row), table$layout$name)
  if (length(panel_gtable_row) != 1L) {
    panel_gtable_row <- grep(sprintf("^panel-%s$", panel_id), table$layout$name)
  }
  if (length(panel_gtable_row) != 1L) {
    return(NULL)
  }

  rel_x <- scales::rescale(x, from = x_range)
  rel_y <- scales::rescale(y, from = y_range)

  list(
    panel_col = table$layout$l[panel_gtable_row] - span_l + 1L,
    panel_row = table$layout$t[panel_gtable_row] - span_t + 1L,
    rel_x = rel_x,
    rel_y = rel_y
  )
}

cross_panel_segments_grob <- function(data, widths, heights, name = NULL) {
  grid::grob(
    data = data,
    widths = widths,
    heights = heights,
    name = name %||% "cross-panel-segments",
    cl = "crossPanelSegmentsGrob"
  )
}

#' @export
drawDetails.crossPanelSegmentsGrob <- function(x, recording = TRUE) {
  total_height_cm <- grid::convertHeight(sum(x$heights), "cm", TRUE)

  for (i in seq_len(nrow(x$data))) {
    row <- x$data[i, , drop = FALSE]

    from_x_cm <- panel_relative_x_cm(x$widths, row$from_col[[1]], row$from_rel_x[[1]])
    to_x_cm <- panel_relative_x_cm(x$widths, row$to_col[[1]], row$to_rel_x[[1]])
    from_y_cm <- panel_relative_y_cm(x$heights, row$from_row[[1]], row$from_rel_y[[1]], total_height_cm)
    to_y_cm <- panel_relative_y_cm(x$heights, row$to_row[[1]], row$to_rel_y[[1]], total_height_cm)

    gp_args <- list(
      col = row$colour[[1]],
      lwd = row$linewidth[[1]],
      lty = row$linetype[[1]]
    )
    if (!is.na(row$alpha[[1]])) {
      gp_args$alpha <- row$alpha[[1]]
    }

    grid::grid.segments(
      x0 = grid::unit(from_x_cm, "cm"),
      y0 = grid::unit(from_y_cm, "cm"),
      x1 = grid::unit(to_x_cm, "cm"),
      y1 = grid::unit(to_y_cm, "cm"),
      gp = do.call(grid::gpar, gp_args)
    )
  }
}

panel_relative_x_cm <- function(widths, panel_col, rel_x) {
  offset_cm <- if (panel_col > 1L) {
    grid::convertWidth(sum(widths[seq_len(panel_col - 1L)]), "cm", TRUE)
  } else {
    0
  }
  panel_width_cm <- grid::convertWidth(widths[panel_col], "cm", TRUE)
  offset_cm + rel_x * panel_width_cm
}

panel_relative_y_cm <- function(heights, panel_row, rel_y, total_height_cm) {
  offset_top_cm <- if (panel_row > 1L) {
    grid::convertHeight(sum(heights[seq_len(panel_row - 1L)]), "cm", TRUE)
  } else {
    0
  }
  panel_height_cm <- grid::convertHeight(heights[panel_row], "cm", TRUE)
  y_from_top_cm <- offset_top_cm + (1 - rel_y) * panel_height_cm
  total_height_cm - y_from_top_cm
}
