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


GeomGeneLabel <- ggproto("GeomGeneLabel", Geom,
  required_aes = c("ymin", "xmin", "xmax", "transcripts", "strand", "track", "label"),
  non_missing_aes = "angle",
  default_aes = aes(
    colour = "black",
    family = "sans",
    size = 3,
    angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, fontface = 1, lineheight = 1.2
  ),
  extra_params = c("exon_height", "na.rm", "x_translation",
    "species", "chr", "subset",
    "label_direction", "label_offset_fraction",
    "link_type",
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
      link_type = "straight"
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
                         exon_height = 0.8,
                         label_direction = "top",
                         label_offset_fraction = 0.3,
                         link_type = "straight") {
    link_type <- match.arg(link_type, c("straight", "elbow"))
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

    # Estimate text width in data coordinates
    # size is in mm; each char ≈ 0.5 × size mm; panel ≈ 150 mm
    data2 <- data2 %>%
      mutate(
        est_nchar = nchar(as.character(.data$label)),
        est_width = .data$est_nchar * .data$size * genomic_range / 600
      )

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

    # Greedy horizontal collision avoidance, independently per position group
    min_gap <- genomic_range * 0.005
    for (pos in unique(data2$label_pos)) {
      idx <- which(data2$label_pos == pos)
      if (length(idx) > 1L) {
        for (j in seq_along(idx)[-1L]) {
          i_curr <- idx[[j]]
          i_prev <- idx[[j - 1L]]
          prev_right <- data2$label_x[[i_prev]] +
            data2$est_width[[i_prev]] / 2 + min_gap
          curr_left  <- data2$label_x[[i_curr]] -
            data2$est_width[[i_curr]] / 2
          if (curr_left < prev_right) {
            new_x <- prev_right + data2$est_width[[i_curr]] / 2
            # Constrain centre labels to stay within their gene span
            if (identical(pos, "center")) {
              new_x <- pmin(new_x, data2$gene_xmax[[i_curr]] -
                data2$est_width[[i_curr]] / 2)
              new_x <- pmax(new_x, data2$gene_xmin[[i_curr]] +
                data2$est_width[[i_curr]] / 2)
            }
            data2$label_x[[i_curr]] <- new_x
          }
        }
      }
    }

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

    if (length(ls_idx) > 0L) {
      leader_start <- data2[ls_idx, , drop = FALSE]
      leader_start$x <- leader_start$orig_x_mid
      leader_start$y <- leader_start$anchor_y
      leader_start_t <- coord$transform(leader_start, panel_params)

      leader_end <- data2[ls_idx, , drop = FALSE]
      leader_end$x <- leader_end$label_x
      leader_end$y <- leader_end$label_y
      leader_end_t <- coord$transform(leader_end, panel_params)

      if (link_type == "straight") {
        lg <- segmentsGrob(
          x0 = leader_start_t$x, y0 = leader_start_t$y,
          x1 = leader_end_t$x,   y1 = leader_end_t$y,
          default.units = "native",
          gp = gpar(col = "grey60", lwd = 0.5)
        )
      } else {
        bend <- data2[ls_idx, , drop = FALSE]
        bend$x <- bend$orig_x_mid
        bend$y <- bend$label_y
        bend_t <- coord$transform(bend, panel_params)
        lg <- gList(
          segmentsGrob(
            x0 = leader_start_t$x, y0 = leader_start_t$y,
            x1 = bend_t$x,         y1 = bend_t$y,
            default.units = "native",
            gp = gpar(col = "grey60", lwd = 0.5)
          ),
          segmentsGrob(
            x0 = bend_t$x,       y0 = bend_t$y,
            x1 = leader_end_t$x, y1 = leader_end_t$y,
            default.units = "native",
            gp = gpar(col = "grey60", lwd = 0.5)
          )
        )
      }
    } else {
      lg <- zeroGrob()
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
  }
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
#' @param link_type Leader line style: `"straight"` (direct line) or
#'   `"elbow"` (right-angle bend via vertical then horizontal segment).
#'   Default `"straight"`. Centre-fitting labels do not draw leader lines.
#' @param species Optional species / individual identifier when `data` is a
#'   `SynSpecies`.
#' @param chr Optional chromosome / seqname restriction when `data` is
#'   Syn-backed.
#' @param subset Optional numeric length-2 genomic window to keep.
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
                           species = NULL, chr = NULL, subset = NULL,
                           inherit.aes = TRUE) {
  params <- Filter(Negate(is.null), c(list(
    ...,
    na.rm = na.rm,
    exon_height = exon_height,
    x_translation = x_translation,
    label_direction = label_direction,
    label_offset_fraction = label_offset_fraction,
    link_type = link_type,
    species = species,
    chr = chr,
    subset = subset
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
