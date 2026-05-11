#' Uniform gene-width scale for multi-track genomic plots
#'
#' `strip_scale()` normalizes gene and intergenic region widths across
#' genomic tracks so that every gene occupies the same visual width and
#' every intergenic gap occupies the same visual width. The track with the
#' most genes determines the shared coordinate system; sparser tracks are
#' aligned within that space.
#'
#' This scale is designed for [`geom_genelabel()`] layers and is mutually
#' exclusive with [`scale_x_ggexon_genomic()`]. It works best with
#' `facet_genomictree(scales = "fixed_x")` — the function will modify the
#' facet to use fixed x scales internally.
#'
#' @param gene_gap_ratio Ratio of gene visual width to intergenic gap width.
#'   When `NULL` (the default), the ratio is derived from the densest
#'   track's actual genomic proportions (median gene width divided by
#'   median gap width).
#' @param align How tracks with fewer genes are positioned within the
#'   shared x-axis. `"left"` packs genes to the left edge, `"right"` to
#'   the right edge, `"center"` centres them.
#'
#' @return A ggexon strip-scale specification, added to the plot with `+`.
#' @export
strip_scale <- function(gene_gap_ratio = NULL,
                        align = c("left", "right", "center")) {
  align <- match.arg(align)
  if (!is.null(gene_gap_ratio)) {
    if (!is.numeric(gene_gap_ratio) ||
        length(gene_gap_ratio) != 1L ||
        is.na(gene_gap_ratio) ||
        gene_gap_ratio <= 0) {
      stop(
        "`gene_gap_ratio` must be a single positive number or `NULL`.",
        call. = FALSE
      )
    }
  }
  structure(
    list(gene_gap_ratio = gene_gap_ratio, align = align),
    class = "ggexon_strip_scale_spec"
  )
}

#' @export
ggplot_add.ggexon_strip_scale_spec <- function(object, plot, object_name) {
  if (!is_ggexon(plot)) {
    stop(
      "`strip_scale()` can only be added to a ggexon plot.",
      call. = FALSE
    )
  }
  if (!is.null(plot@genomic_x_scale)) {
    stop(
      "`strip_scale()` and `scale_x_ggexon_genomic()` are mutually exclusive.",
      call. = FALSE
    )
  }
  if (!is.null(plot@strip_scale)) {
    warning("`strip_scale()` is already set on this plot; replacing it.",
            call. = FALSE)
  }
  plot@strip_scale <- object
  plot
}

# ── Build-pipeline integration ──────────────────────────────────────────

apply_strip_scale <- function(data, layers, strip_scale_spec, layout, plot) {
  gene_layers <- .strip_scale_gene_layers(layers)
  if (length(gene_layers) == 0L) {
    stop(
      "`strip_scale()` requires at least one `geom_genelabel()` layer.",
      call. = FALSE
    )
  }

  gene_intervals <- .strip_scale_collect_intervals(data, gene_layers)
  if (nrow(gene_intervals) == 0L) {
    stop(
      "`strip_scale()` found no gene-level data in genelabel layers.",
      call. = FALSE
    )
  }

  ratio <- .strip_scale_resolve_ratio(
    gene_intervals,
    strip_scale_spec$gene_gap_ratio
  )
  transforms <- .strip_scale_build_transforms(
    gene_intervals = gene_intervals,
    ratio = ratio,
    align = strip_scale_spec$align
  )

  data <- lapply(seq_along(data), function(i) {
    .strip_scale_transform_layer(data[[i]], transforms)
  })

  layout <- .strip_scale_force_fixed_x(layout, names(transforms))

  list(data = data, layout = layout, transforms = transforms)
}

.strip_scale_gene_layers <- function(layers) {
  which(vapply(layers, function(l) {
    identical(l$geom, GeomGeneLabel)
  }, logical(1)))
}

.strip_scale_collect_intervals <- function(data, gene_layers) {
  pieces <- list()
  for (i in gene_layers) {
    df <- data[[i]]
    if (!is.data.frame(df) ||
        nrow(df) == 0L ||
        !all(c("PANEL", "transcripts", "xmin", "xmax", "track") %in% names(df))) {
      next
    }

    gene_bounds <- df %>%
      dplyr::mutate(
        PANEL = ggexon_panel_id(.data$PANEL),
        gene_start = pmin(as.numeric(.data$xmin), as.numeric(.data$xmax)),
        gene_end   = pmax(as.numeric(.data$xmin), as.numeric(.data$xmax))
      ) %>%
      dplyr::filter(
        is.finite(.data$gene_start),
        is.finite(.data$gene_end),
        .data$gene_end > .data$gene_start
      ) %>%
      dplyr::group_by(.data$PANEL, .data$track, .data$transcripts) %>%
      dplyr::summarise(
        gene_start = min(.data$gene_start, na.rm = TRUE),
        gene_end   = max(.data$gene_end,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$PANEL, .data$track, .data$gene_start)

    if (nrow(gene_bounds) == 0L) next
    pieces[[length(pieces) + 1L]] <- gene_bounds
  }

  intervals <- dplyr::bind_rows(pieces)
  if (is.null(intervals) || nrow(intervals) == 0L) {
    return(data.frame())
  }
  dplyr::distinct(intervals)
}

.strip_scale_resolve_ratio <- function(gene_intervals,
                                       gene_gap_ratio = NULL) {
  if (!is.null(gene_gap_ratio)) {
    return(as.numeric(gene_gap_ratio))
  }

  gene_counts <- dplyr::count(gene_intervals, .data$PANEL, name = "n_genes")
  if (nrow(gene_counts) == 0L) return(3)

  densest_panel <- gene_counts$PANEL[which.max(gene_counts$n_genes)]
  densest_genes <- gene_intervals[
    gene_intervals$PANEL == densest_panel, , drop = FALSE
  ]
  densest_genes <- densest_genes[
    order(densest_genes$gene_start), , drop = FALSE
  ]

  gene_widths <- densest_genes$gene_end - densest_genes$gene_start
  gene_widths <- gene_widths[is.finite(gene_widths) & gene_widths > 0]
  gap_widths <- pmax(
    densest_genes$gene_start[-1L] - densest_genes$gene_end[-nrow(densest_genes)],
    0
  )
  gap_widths <- gap_widths[is.finite(gap_widths) & gap_widths > 0]

  med_gene <- stats::median(gene_widths, na.rm = TRUE)
  med_gap <- stats::median(gap_widths, na.rm = TRUE)

  if (!is.finite(med_gene) || med_gene <= 0) return(3)
  if (!is.finite(med_gap) || med_gap <= 0) return(max(med_gene / 1000, 3))

  max(med_gene / med_gap, 0.1)
}

# Merge overlapping gene intervals so that each genomic position maps to
# exactly one segment in the piecewise transform.
.strip_scale_merge_intervals <- function(starts, ends) {
  if (length(starts) == 0L) {
    return(data.frame(start = numeric(0), end = numeric(0)))
  }
  ord <- order(starts, ends)
  starts <- starts[ord]
  ends   <- ends[ord]

  out_starts <- numeric(0)
  out_ends   <- numeric(0)
  cur_start  <- starts[[1L]]
  cur_end    <- ends[[1L]]

  for (i in seq_along(starts)[-1L]) {
    if (starts[[i]] <= cur_end) {
      cur_end <- max(cur_end, ends[[i]])
    } else {
      out_starts <- c(out_starts, cur_start)
      out_ends   <- c(out_ends,   cur_end)
      cur_start  <- starts[[i]]
      cur_end    <- ends[[i]]
    }
  }
  out_starts <- c(out_starts, cur_start)
  out_ends   <- c(out_ends,   cur_end)
  data.frame(start = out_starts, end = out_ends)
}

.strip_scale_build_transforms <- function(gene_intervals,
                                          ratio,
                                          align = "left") {
  gene_intervals <- gene_intervals[
    order(gene_intervals$PANEL, gene_intervals$gene_start),
    ,
    drop = FALSE
  ]

  gene_counts <- dplyr::count(gene_intervals, .data$PANEL, name = "n_genes")
  if (nrow(gene_counts) == 0L) return(list())
  N_max <- max(gene_counts$n_genes)

  g <- ratio
  p <- 1
  total_span <- N_max * g + max(N_max - 1L, 0L) * p

  transforms <- list()
  for (panel_id in unique(gene_intervals$PANEL)) {
    panel_genes <- gene_intervals[
      gene_intervals$PANEL == panel_id, , drop = FALSE
    ]
    panel_genes <- panel_genes[order(panel_genes$gene_start), , drop = FALSE]

    # Merge overlapping genes into disjoint blocks.  Each block gets one
    # visual "gene" slot; within-block sub-genes keep their proportional
    # positions thanks to the merged interval's slope.
    merged <- .strip_scale_merge_intervals(
      panel_genes$gene_start,
      panel_genes$gene_end
    )
    M <- nrow(merged)
    if (M == 0L) next

    track_span <- M * g + max(M - 1L, 0L) * p
    offset <- switch(
      align,
      left   = 0,
      right  = total_span - track_span,
      center = (total_span - track_span) / 2
    )
    offset <- max(offset, 0)

    pieces <- list()
    current_visual <- offset

    for (i in seq_len(M)) {
      block <- merged[i, , drop = FALSE]
      bw <- block$end - block$start
      if (bw > 0) {
        pieces[[length(pieces) + 1L]] <- data.frame(
          genomic_start = block$start,
          genomic_end   = block$end,
          plot_start    = current_visual,
          plot_end      = current_visual + g,
          slope         = g / bw,
          region_type   = "gene",
          stringsAsFactors = FALSE
        )
      }
      current_visual <- current_visual + g

      if (i < M) {
        next_block <- merged[i + 1L, , drop = FALSE]
        gap_start <- block$end
        gap_end   <- next_block$start
        gap_w     <- gap_end - gap_start
        if (gap_w > 0) {
          pieces[[length(pieces) + 1L]] <- data.frame(
            genomic_start = gap_start,
            genomic_end   = gap_end,
            plot_start    = current_visual,
            plot_end      = current_visual + p,
            slope         = p / gap_w,
            region_type   = "gap",
            stringsAsFactors = FALSE
          )
        }
        current_visual <- current_visual + p
      }
    }

    transform <- do.call(rbind, pieces)
    if (nrow(transform) > 0L) {
      transform$PANEL <- panel_id
      rownames(transform) <- NULL
      transforms[[as.character(panel_id)]] <- transform
    }
  }

  transforms
}

.strip_scale_transform_layer <- function(layer_data, transforms) {
  if (!is.data.frame(layer_data) || length(transforms) == 0L) {
    return(layer_data)
  }

  x_columns <- intersect(c("x", "xmin", "xmax", "xend", "xintercept"),
                         names(layer_data))
  if (length(x_columns) == 0L || !"PANEL" %in% names(layer_data)) {
    return(layer_data)
  }

  panel_ids <- ggexon_panel_id(layer_data$PANEL)
  for (panel_id in unique(panel_ids)) {
    key <- as.character(panel_id)
    transform <- transforms[[key]]
    if (is.null(transform)) next

    idx <- which(panel_ids == panel_id)
    for (col in x_columns) {
      genomic_col <- paste0("genomic_", col)
      if (!genomic_col %in% names(layer_data)) {
        layer_data[[genomic_col]] <- layer_data[[col]]
      }
      layer_data[[col]][idx] <- strip_scale_to_plot_x(
        layer_data[[genomic_col]][idx],
        transform
      )
    }
  }

  layer_data
}

strip_scale_to_plot_x <- function(x, transform) {
  vapply(as.numeric(x), function(value) {
    if (!is.finite(value) || is.null(transform) || nrow(transform) == 0L) {
      return(value)
    }
    first <- transform[1L, , drop = FALSE]
    last  <- transform[nrow(transform), , drop = FALSE]

    # Extrapolate linearly outside the transformed range
    if (value < first$genomic_start[[1L]]) {
      return(first$plot_start[[1L]] - (first$genomic_start[[1L]] - value))
    }
    if (value > last$genomic_end[[1L]]) {
      return(last$plot_end[[1L]] + (value - last$genomic_end[[1L]]))
    }

    # Find all segments that cover this genomic position
    idx <- which(value >= transform$genomic_start &
                 value <= transform$genomic_end)

    # Safety: if no segment covers this point, use the nearest one.
    # This can happen only when the transform has gaps (shouldn't after
    # interval merging, but guards against unexpected data).
    if (length(idx) == 0L) {
      dists <- abs(value - transform$genomic_start)
      idx <- which.min(dists)
      if (length(idx) == 0L) {
        return(value)  # fallback: return original
      }
    }

    # When multiple segments cover the same point (overlapping intervals
    # before merging), use the first one (earliest in genomic order).
    idx <- idx[[1L]]
    row <- transform[idx, , drop = FALSE]
    row$plot_start[[1L]] + (value - row$genomic_start[[1L]]) * row$slope[[1L]]
  }, numeric(1))
}

.strip_scale_force_fixed_x <- function(layout, transform_panels) {
  layout_df <- layout$layout
  if (!is.data.frame(layout_df) || !"PANEL" %in% names(layout_df)) {
    return(layout)
  }

  panel_ids <- as.character(unique(layout_df$PANEL))
  target_panels <- intersect(panel_ids, transform_panels)
  if (length(target_panels) == 0L) {
    return(layout)
  }

  layout_df$SCALE_X <- as.integer(layout_df$SCALE_X)
  layout_df$SCALE_X[as.character(layout_df$PANEL) %in% target_panels] <- 1L

  if (!is.null(layout$facet) &&
      !is.null(layout$facet$params) &&
      !is.null(layout$facet$params$free)) {
    layout$facet$params$free$x <- FALSE
  }

  layout$layout <- layout_df
  layout
}