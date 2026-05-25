#' Parse colon-delimited label_direction string into ordered position vector
#'
#' Splits a label_direction string like `"bottom:top:center"` into
#' `c("bottom", "top", "center")`. Valid tokens are `"top"`, `"bottom"`,
#' and `"center"`.
#'
#' @param label_direction Character string, possibly colon-delimited.
#' @return Character vector of valid position tokens in the order they
#'   appear in the input.
#' @keywords internal
.parse_label_positions <- function(label_direction) {
  if (is.null(label_direction) || length(label_direction) != 1L || is.na(label_direction) || !nzchar(label_direction)) {
    return(c("top"))
  }
  parts <- strsplit(as.character(label_direction), ":")[[1L]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  valid <- c("top", "bottom", "center")
  bad <- setdiff(parts, valid)
  if (length(bad) > 0L) {
    stop(
      "Invalid label position(s): ", paste(bad, collapse = ", "),
      ". Valid positions are: ", paste(valid, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(parts) == 0L) {
    return(c("top"))
  }
  parts
}

#' Collapse consecutive genes with identical labels into tandem groups
#'
#' For each track, genes are ordered by genomic position. Consecutive
#' genes that share the same \code{label} are merged into a single label
#' row spanning the full tandem array. Member positions are stored as an
#' attribute for connector drawing.
#'
#' @param data2 A data frame with one row per gene, already sorted by
#'   \code{orig_x_mid} within each track.
#' @return A data frame with collapsed tandem rows plus a
#'   \code{"tandem_anchors"} attribute.
#' @keywords internal
.collapse_tandem_labels <- function(data2) {
  if (nrow(data2) <= 1L) {
    attr(data2, "tandem_anchors") <- list()
    return(data2)
  }

  data2 <- data2 %>%
    dplyr::group_by(.data$track) %>%
    dplyr::mutate(
      same_as_prev = .data$label == dplyr::lag(.data$label, default = ""),
      run_id = cumsum(!.data$same_as_prev)
    ) %>%
    dplyr::ungroup()

  tandem_anchors <- list()
  collapsed_rows <- list()
  next_id <- 1L

  run_key <- paste(as.character(data2$track), data2$run_id, sep = "\r")
  for (run in base::split(data2, run_key)) {
    if (nrow(run) == 1L) {
      collapsed_rows[[length(collapsed_rows) + 1L]] <- run
    } else {
      merged <- run[1L, , drop = FALSE]
      merged$gene_xmin <- min(run$gene_xmin, na.rm = TRUE)
      merged$gene_xmax <- max(run$gene_xmax, na.rm = TRUE)
      merged$orig_x_mid <- (merged$gene_xmin + merged$gene_xmax) / 2
      merged$tandem_id <- next_id

      tandem_anchors[[as.character(next_id)]] <- data.frame(
        x = run$orig_x_mid,
        gene_ymax = run$gene_ymax,
        gene_ymin = run$gene_ymin,
        gene_ymid = run$gene_ymid,
        stringsAsFactors = FALSE
      )
      next_id <- next_id + 1L

      collapsed_rows[[length(collapsed_rows) + 1L]] <- merged
    }
  }

  data2 <- dplyr::bind_rows(collapsed_rows)
  if (!"tandem_id" %in% names(data2)) {
    data2$tandem_id <- NA_integer_
  }
  attr(data2, "tandem_anchors") <- tandem_anchors
  data2
}


GeomGeneLabel <- ggproto("GeomGeneLabel", Geom,
  required_aes = c("ymin", "xmin", "xmax", "transcripts", "strand", "track", "label"),
  non_missing_aes = "angle",
  default_aes = aes(
    colour = "black",
    family = "sans",
    size = 3,
    angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, fontface = 1, lineheight = 1.2,
    reference_gene = NA_character_,
    reference_gene_name = NA_character_,
    homology_hit = NA
  ),
  extra_params = c("exon_height", "na.rm", "x_translation",
    "species", "chr", "subset",
    "label_direction", "label_offset_fraction",
    "link_type", "collapse_tandem", "show_link",
    "panel_width_mm", "panel_width_inch",
    fontface = 1, lineheight = 1.2
  ),
  default_params = function() {
    list(
      exon_height = 0.8,
      x_translation = 0,
      species = NULL,
      chr = NULL,
      subset = NULL,
      label_direction = "top",
      label_offset_fraction = 0.3,
      link_type = "straight",
      collapse_tandem = FALSE,
      show_link = TRUE,
      panel_width_mm = NULL,
      panel_width_inch = NULL
    )
  },
  setup_data = function(data, params) {
    data <- GeomExon$setup_data(data, params)
    exon_height <- params$exon_height %||% 0.8
    label_offset <- exon_height *
      (params$label_offset_fraction %||% 0.3)
    label_direction <- params$label_direction %||% "top"
    positions <- .parse_label_positions(label_direction)

    # Expand annotation space when top or center (possible fallback) or
    # bottom are present in the position set.
    if ("top" %in% positions || "center" %in% positions) {
      data$ymax <- data$ymax + label_offset
    }
    if ("bottom" %in% positions) {
      data$ymin <- data$ymin - label_offset
    }
    data
  },

  draw_panel = function(data, panel_params, coord, check_overlap = FALSE,
                         show_link = TRUE,
                         exon_height = 0.8,
                         label_direction = "top",
                         label_offset_fraction = 0.3,
                         link_type = "straight",
                         collapse_tandem = FALSE,
                         panel_width_mm = NULL,
                         panel_width_inch = NULL) {
    link_type <- match.arg(link_type, c("straight", "elbow", "spline"))
    label_offset <- exon_height * label_offset_fraction
    positions <- .parse_label_positions(label_direction)

    genomic_range <- diff(range(c(data$xmin, data$xmax), na.rm = TRUE))
    if (genomic_range <= 0) genomic_range <- 1

    # Collapse one row per gene (transcript)
    data2 <- data %>%
      group_by(.data$transcripts) %>%
      mutate(
        gene_xmin = min(.data$xmin),
        gene_xmax = max(.data$xmax),
        orig_x_mid = (.data$gene_xmax + .data$gene_xmin) / 2,
        gene_ymax = .data$ymax,
        gene_ymin = .data$ymin,
        gene_ymid = (.data$ymax + .data$ymin) / 2
      ) %>%
      dplyr::slice(1) %>%
      ungroup() %>%
      arrange(.data$orig_x_mid)

    if (nrow(data2) == 0L) {
      return(zeroGrob())
    }

    # Estimate text width in data coordinates:
    #   text_width_data = text_width_mm × (genomic_range / panel_width_mm)
    #   text_width_mm    = nchar × 0.5 × size
    # → est_width = nchar × 0.5 × size × genomic_range / panel_width_mm
    # panel_width_inch overrides panel_width_mm when both are provided.
    panel_mm <- if (!is.null(panel_width_inch)) panel_width_inch * 25.4 else panel_width_mm %||% 300
    if (!is.numeric(panel_mm) || panel_mm <= 0) panel_mm <- 300
    data2 <- data2 %>%
      mutate(
        est_nchar = nchar(as.character(.data$label)),
        est_width = .data$est_nchar * 0.5 * .data$size * genomic_range / panel_mm
      )

    # Collapse tandem duplications: consecutive genes with identical labels
    # share a single label and connector bracket.
    if (isTRUE(collapse_tandem)) {
      data2 <- .collapse_tandem_labels(data2)
    }
    tandem_anchors <- attr(data2, "tandem_anchors") %||% list()

    if (nrow(data2) == 0L) {
      return(zeroGrob())
    }

    # Assign gene index within each track; resolve label position via modulo
    data2 <- data2 %>%
      group_by(.data$track) %>%
      mutate(
        gene_index = dplyr::row_number(),
        pos_idx = (.data$gene_index - 1L) %% length(positions) + 1L,
        label_pos = positions[.data$pos_idx]
      ) %>%
      ungroup()

    # Center-position genes: check whether the label fits inside the gene tag.
    # Genes that don't fit fall back to "top".
    data2 <- data2 %>%
      mutate(
        fits_in_gene = .data$label_pos == "center" &
          .data$est_width <= (.data$gene_xmax - .data$gene_xmin),
        label_pos = if_else(
          .data$label_pos == "center" & !.data$fits_in_gene,
          "top",
          .data$label_pos
        )
      )

    # Initial horizontal label placement at gene centre
    data2 <- data2 %>%
      mutate(label_x = .data$orig_x_mid)

    # Minimum-displacement collision avoidance (alternating projections)
    min_gap <- genomic_range * 0.005
    for (pos in unique(data2$label_pos)) {
      idx <- which(data2$label_pos == pos)
      n <- length(idx)
      if (n <= 1L) next

      ideal <- data2$label_x[idx]
      halfw <- data2$est_width[idx] / 2
      D <- halfw[-n] + halfw[-1L] + min_gap

      x <- ideal
      changed <- TRUE
      max_iter <- 50L
      iter <- 0L
      while (changed && iter < max_iter) {
        changed <- FALSE
        iter <- iter + 1L

        # Forward pass: enforce x[i+1] >= x[i] + D[i]
        for (j in 2L:n) {
          d <- D[[j - 1L]]
          if (x[[j]] < x[[j - 1L]] + d) {
            delta <- (x[[j - 1L]] + d - x[[j]]) / 2
            x[[j - 1L]] <- x[[j - 1L]] - delta
            x[[j]]      <- x[[j]] + delta
            changed <- TRUE
          }
        }

        # Backward pass
        for (j in seq.int(n - 1L, 1L)) {
          d <- D[[j]]
          if (x[[j + 1L]] < x[[j]] + d) {
            delta <- (x[[j]] + d - x[[j + 1L]]) / 2
            x[[j]]      <- x[[j]] - delta
            x[[j + 1L]] <- x[[j + 1L]] + delta
            changed <- TRUE
          }
        }
      }

      # Constrain centre labels to stay within their gene span
      if (identical(pos, "center")) {
        for (j in seq_len(n)) {
          i <- idx[[j]]
          x[[j]] <- pmin(x[[j]], data2$gene_xmax[[i]] - halfw[[j]])
          x[[j]] <- pmax(x[[j]], data2$gene_xmin[[i]] + halfw[[j]])
        }
      }

      data2$label_x[idx] <- x
    }

    # Constrain all labels to stay within the data range to prevent
    # horizontal overflow when the plot is saved at non-standard dimensions
    data_xmin <- min(data$xmin, na.rm = TRUE)
    data_xmax <- max(data$xmax, na.rm = TRUE)
    halfw <- data2$est_width / 2
    data2$label_x <- pmax(data2$label_x, data_xmin + halfw)
    data2$label_x <- pmin(data2$label_x, data_xmax - halfw)

    # Per-position label y and anchor y
    top_label_y    <- if ("top"    %in% data2$label_pos) max(data2$gene_ymax) else NA_real_
    bottom_label_y <- if ("bottom" %in% data2$label_pos) min(data2$gene_ymin) else NA_real_

    data2 <- data2 %>%
      mutate(
        label_y = dplyr::case_when(
          .data$label_pos == "top"    ~ top_label_y,
          .data$label_pos == "bottom" ~ bottom_label_y,
          .data$label_pos == "center" ~ .data$gene_ymid,
          TRUE                        ~ top_label_y
        ),
        anchor_y = dplyr::case_when(
          .data$label_pos == "top"    ~ .data$gene_ymax - label_offset,
          .data$label_pos == "bottom" ~ .data$gene_ymin + label_offset,
          .data$label_pos == "center" ~ .data$gene_ymid,
          TRUE                        ~ .data$gene_ymax - label_offset
        ),
        vjust = dplyr::case_when(
          .data$label_pos == "top"    ~ 1,
          .data$label_pos == "bottom" ~ 0,
          .data$label_pos == "center" ~ 0.5,
          TRUE                        ~ 1
        )
      )

    # Coordinate transform for labels
    label_data <- data2
    label_data$x <- label_data$label_x
    label_data$y <- label_data$label_y
    label_t <- coord$transform(label_data, panel_params)

    # Leader lines — skip for centre labels that fit inside the gene
    has_leader <- data2$label_pos != "center" | !data2$fits_in_gene
    ls_idx <- which(has_leader)

    lg <- zeroGrob()
    if (isTRUE(show_link)) {
      leader_grobs <- list()

      if (length(ls_idx) > 0L) {
        # Split leader rows into tandem and non-tandem
        is_tandem <- !is.na(data2$tandem_id[ls_idx])
        singletons <- ls_idx[!is_tandem]
        tandems    <- ls_idx[is_tandem]

        # ---- non-tandem leader lines ----
        if (length(singletons) > 0L) {
          leader_start <- data2[singletons, , drop = FALSE]
          leader_start$x <- leader_start$orig_x_mid
          leader_start$y <- leader_start$anchor_y
          leader_start_t <- coord$transform(leader_start, panel_params)

          leader_end <- data2[singletons, , drop = FALSE]
          leader_end$x <- leader_end$label_x
          leader_end$y <- leader_end$label_y
          leader_end_t <- coord$transform(leader_end, panel_params)

          link_data <- data.frame(
            x = leader_start_t$x,
            y = leader_start_t$y,
            xend = leader_end_t$x,
            yend = leader_end_t$y,
            colour = "grey60",
            linewidth = 0.5,
            linetype = "solid",
            alpha = NA_real_,
            stringsAsFactors = FALSE
          )
          leader_grobs[[length(leader_grobs) + 1L]] <- .draw_link_grobs_raw(link_data, link_type)
        }

        # ---- tandem bracket connectors ----
        for (i in tandems) {
          tid <- data2$tandem_id[[i]]
          members <- tandem_anchors[[as.character(tid)]]
          if (is.null(members) || nrow(members) < 2L) {
            # Fallback: draw as regular singleton leader
            leader_s <- data2[i, , drop = FALSE]
            leader_s$x <- leader_s$orig_x_mid
            leader_s$y <- leader_s$anchor_y
            leader_s_t <- coord$transform(leader_s, panel_params)
            leader_e <- data2[i, , drop = FALSE]
            leader_e$x <- leader_e$label_x
            leader_e$y <- leader_e$label_y
            leader_e_t <- coord$transform(leader_e, panel_params)

            link_data <- data.frame(
              x = leader_s_t$x,
              y = leader_s_t$y,
              xend = leader_e_t$x,
              yend = leader_e_t$y,
              colour = "grey60",
              linewidth = 0.5,
              linetype = "solid",
              alpha = NA_real_,
              stringsAsFactors = FALSE
            )
            leader_grobs[[length(leader_grobs) + 1L]] <- .draw_link_grobs_raw(link_data, link_type)
            next
          }

          label_pos <- data2$label_pos[[i]]
          # Compute each member's anchor y based on the shared label position
          members$anchor_y <- if (identical(label_pos, "top")) {
            members$gene_ymax - label_offset
          } else if (identical(label_pos, "bottom")) {
            members$gene_ymin + label_offset
          } else if (identical(label_pos, "center")) {
            members$gene_ymid
          } else {
            members$gene_ymax - label_offset
          }

          bracket_y <- mean(range(members$anchor_y, na.rm = TRUE))

          # Horizontal bracket at mean anchor y — use data-space x
          bracket_df <- data.frame(
            x = c(members$x[[1L]], members$x[[nrow(members)]]),
            y = c(bracket_y, bracket_y),
            stringsAsFactors = FALSE
          )
          bracket_df_t <- coord$transform(bracket_df, panel_params)

          leader_grobs[[length(leader_grobs) + 1L]] <- segmentsGrob(
            x0 = bracket_df_t$x[[1L]], y0 = bracket_df_t$y[[1L]],
            x1 = bracket_df_t$x[[2L]], y1 = bracket_df_t$y[[2L]],
            default.units = "native",
            gp = gpar(col = "grey60", lwd = 0.5)
          )

          # Vertical drops from bracket to each gene anchor
          for (k in seq_len(nrow(members))) {
            drop_df <- data.frame(
              x = rep(members$x[[k]], 2L),
              y = c(bracket_y, members$anchor_y[[k]]),
              stringsAsFactors = FALSE
            )
            drop_df_t <- coord$transform(drop_df, panel_params)
            leader_grobs[[length(leader_grobs) + 1L]] <- segmentsGrob(
              x0 = drop_df_t$x[[1L]], y0 = drop_df_t$y[[1L]],
              x1 = drop_df_t$x[[2L]], y1 = drop_df_t$y[[2L]],
              default.units = "native",
              gp = gpar(col = "grey60", lwd = 0.3)
            )
          }

          # Main vertical leader from bracket centre to label
          bracket_mid_x <- mean(range(members$x))
          label_point <- data2[i, , drop = FALSE]
          label_point$x <- label_point$label_x
          label_point$y <- label_point$label_y
          label_point_t <- coord$transform(label_point, panel_params)

          leader_grobs[[length(leader_grobs) + 1L]] <- segmentsGrob(
            x0 = grid::unit(bracket_mid_x, "native"),
            y0 = bracket_df_t$y[[1L]],
            x1 = label_point_t$x,
            y1 = label_point_t$y,
            default.units = "native",
            gp = gpar(col = "grey60", lwd = 0.5)
          )
        }
      }

      if (length(leader_grobs) == 0L) {
        lg <- zeroGrob()
      } else if (length(leader_grobs) == 1L) {
        lg <- leader_grobs[[1L]]
      } else {
        lg <- do.call(gList, leader_grobs)
      }
    }

    tg <- textGrob(
      label_t$label, label_t$x, label_t$y,
      default.units = "native",
      hjust = label_t$hjust, vjust = label_t$vjust,
      rot = label_t$angle,
      gp = gpar(
        col = alpha(label_t$colour, label_t$alpha),
        fontsize = label_t$size * .pt,
        fontfamily = label_t$family,
        fontface = label_t$fontface,
        lineheight = label_t$lineheight
      ),
      check.overlap = check_overlap
    )

    gList(lg, tg)
  },
  syn_data = function(x, layer) {
    params <- syn_layer_params(layer)
    context <- layer$syn_plot_context %||% NULL
    syn_to_gene_df(
      x = x,
      species = params$species,
      chr = params$chr,
      subset = params$subset,
      context = context
    )
  },
  syn_default_aes = c(
    "xmin", "xmax", "ymin", "transcripts", "strand", "track", "label",
    "group", "reference_gene", "reference_gene_name", "homology_hit"
  )
)

#' Draw gene labels on exon tracks
#'
#' `geom_genelabel()` places one text label per transcript or gene span on an
#' exon-style genomic track. The `label_direction` parameter accepts
#' colon-delimited combinations to distribute labels across positions using
#' modulo assignment.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param x_translation Optional x offset applied before drawing.
#' @param exon_height Optional exon rectangle height used when preparing track
#'   coordinates.
#' @param label_direction One or more label positions joined with `:`, e.g.
#'   `"top"`, `"bottom"`, `"top:bottom"`, or `"bottom:top:center"`.
#'   Each gene receives a position based on its track index modulo the number
#'   of position tokens:
#'
#'   * `"top"` — all labels above the highest track.
#'   * `"bottom"` — all labels below the lowest track.
#'   * `"top:bottom"` — odd-indexed genes above, even-indexed below.
#'   * `"bottom:top:center"` — gene 1 bottom, gene 2 top, gene 3 centre (and
#'     repeats). Genes assigned `"center"` have their label placed on the
#'     gene body. If the label text is wider than the gene span, the label
#'     falls back to `"top"`.
#'
#'   Valid tokens: `"top"`, `"bottom"`, `"center"`. Default `"top"`.
#' @param label_offset_fraction Distance between the exon tracks and the label
#'   line, expressed as a fraction of `exon_height`. Default `0.3`.
#' @param link_type Leader line style: `"straight"` (direct line),
#'   `"elbow"` (right-angle bend via vertical then horizontal segment),
#'   or `"spline"` (smooth Bézier curve). Default `"straight"`.
#'   Centre-fitting labels do not draw leader lines.
#' @param collapse_tandem When `TRUE`, consecutive genes with identical labels
#'   (tandem duplications) share a single label connected to all gene bodies
#'   by a bracket-style connector. Default `FALSE`.
#' @param show_link When `TRUE` (the default), leader lines are drawn between
#'   gene bodies and labels. Set to `FALSE` to suppress all leader lines
#'   (only the text labels are rendered).
#' @param species Optional species / individual identifier when `data` is a
#'   `SynSpecies`.
#' @param chr Optional chromosome / seqname restriction when `data` is
#'   Syn-backed.
#' @param subset Optional numeric length-2 genomic window to keep.
#' @param panel_width_mm Estimated width of the genomic panel in millimetres.
#'   Used to convert text size into data-coordinate units for label placement
#'   and collision avoidance. Default `300` (≈ A4/US-letter panel width).
#'   Increase this for wide output (e.g. `ggsave(width = 40)`).
#' @param panel_width_inch Same as `panel_width_mm` but in inches. When both
#'   are provided, `panel_width_inch` takes precedence. One inch = 25.4 mm.
#'
#' @return A ggplot2 layer using the internal `GeomGeneLabel` ggproto.
#' @export
geom_genelabel <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           x_translation = NULL,
                           ..., na.rm = FALSE, show.legend = NA,
                           exon_height = NULL,
                           label_direction = NULL,
                           label_offset_fraction = NULL,
                           link_type = NULL,
                           collapse_tandem = NULL,
                           show_link = NULL,
                           species = NULL, chr = NULL, subset = NULL,
                           panel_width_mm = NULL,
                           panel_width_inch = NULL,
                           inherit.aes = TRUE) {
  params <- Filter(Negate(is.null), c(list(
    ...,
    na.rm = na.rm,
    exon_height = exon_height,
    x_translation = x_translation,
    label_direction = label_direction,
    label_offset_fraction = label_offset_fraction,
    link_type = link_type,
    collapse_tandem = collapse_tandem,
    show_link = show_link,
    species = species,
    chr = chr,
    subset = subset,
    panel_width_mm = panel_width_mm,
    panel_width_inch = panel_width_inch
  )))
  layer(
    data = data,
    mapping = mapping,
    geom = GeomGeneLabel,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = params
  )
}
