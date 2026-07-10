#' Geom implementation for exon features
#'
#' Internal ggproto used by [`geom_exon()`].
GeomExon <- ggproto("GeomExon", Geom,
                      required_aes = c("ymin", "xmin", "xmax", "transcripts","strand", "track", "type"),
                      non_missing_aes = c("linewidth", "shape"),
                      extra_params = c("exon_height", "transcript_backbone_ratio", "transcript_arrow_ratio",
                                       "transcript_arrow_length", "transcript_backbone_fill",
                                       "transcript_backbone_colour", "na.rm", "x_translation", "subset", "annotation_type",
                                       "breakdata", "species", "chr", "force_flat"),
                      default_aes = aes(linewidth = 0, linejoin = "mitre", fill="black",
                        colour = NULL,
                        size = 15,
                        linetype = 1,
                        shape = 19,
                        alpha = NA,
                        stroke = 1
                      ),

                    setup_data = function(data, params){
                      x_translation <- if (is.null(params$x_translation)) 0 else params$x_translation
                      exon_height <- if (is.null(params$exon_height)) 0.8 else params$exon_height
                      if (!"blank_panel" %in% names(data)) {
                        data$blank_panel <- FALSE
                      }
                      if ("transcripts" %in% names(data)) {
                        blank_transcript <- grepl("^__blank__", as.character(data$transcripts))
                        data$blank_panel <- as.logical(data$blank_panel) | blank_transcript
                      }

                      if (!is.null(params$annotation_type)){
                        data = data %>% filter(type == params$annotation_type)
                      }
                      if (!is.null(params$subset)) {
                        # Filter based on the requested subset region.
                        start1 = int(params$subset[1])
                        end1 = int(params$subset[2])
                        data = data %>% filter(xmin >= start1, xmax <= end1)
                      }
                      if (!is.null(params$breakdata)){
                        data = addbreak(data, params$breakdata)
                      }

                      data = data %>% group_by(track) %>%
                        mutate(x_adjustment = 0)



                      if (x_translation != 0){
                        data = data %>% mutate(xmin = xmin + x_translation, xmax =xmax + x_translation)
                      }

                      if (isTRUE(params$force_flat)) {
                        data$ymin <- 1
                      }

                      rec_data = seq_add_y(data = data,
                                           track_proportion = params$transcripts_track_ratio,
                                           exon_proportion = 0.8, blank_proportion = 0.2,
                                           sandwich_ratio = params$sandwich_ratio,
                                           exon_height = exon_height)


                    },

                      draw_panel = function(data, panel_params, coord, flipped_aes = FALSE,
                                            transcript_backbone_ratio = 0.1,
                                            transcript_arrow_ratio = 0.25,
                                            transcript_arrow_length = 160,
                                            transcript_backbone_fill = NULL,
                                            transcript_backbone_colour = NULL){
                        blank_flags <- if ("blank_panel" %in% names(data)) {
                          isTRUE(data$blank_panel) | (data$blank_panel %in% TRUE)
                        } else {
                          rep(FALSE, nrow(data))
                        }
                        visible_data <- data[!blank_flags, , drop = FALSE]
                        if (nrow(visible_data) == 0L) {
                          return(zeroGrob())
                        }
                        track_data = add_transcripts_seq_line(visible_data)
                        track_rect_data = add_transcripts_seq_rect(
                          track_data,
                          backbone_ratio = transcript_backbone_ratio %||% 0.1
                        )
                        track_rect_data <- .apply_transcript_backbone_aes(
                          track_rect_data,
                          fill = transcript_backbone_fill,
                          colour = transcript_backbone_colour
                        )
                        transcripts_line_Grob = ggplot2::GeomRect$draw_panel(track_rect_data, panel_params, coord)
                        tri_data = add_transcripts_direction(
                          track_data,
                          ratio = transcript_arrow_ratio %||% 0.25,
                          lengthABS = transcript_arrow_length %||% 160
                        )
                        tri_data$linewidth = 0
                        tri_data <- .apply_transcript_backbone_aes(
                          tri_data,
                          fill = transcript_backbone_fill,
                          colour = transcript_backbone_colour
                        )
                        transcripts_tri_Grob = GeomPolygon$draw_panel(tri_data, panel_params, coord)
                        #print(getAnywhere("GeomRect"))
                        exon_Grob = ggplot2::GeomRect$draw_panel(visible_data, panel_params, coord)
                        ggname("geom_exon", gTree(children = gList(
                          transcripts_line_Grob,
                          exon_Grob,
                          transcripts_tri_Grob
                            )
                          )
                        )
                      },
                    default_params = function() {
                      list(
                        exon_height = 0.8,
                        transcript_backbone_ratio = 0.1,
                        transcript_arrow_ratio = 0.25,
                        transcript_arrow_length = 160,
                        transcript_backbone_fill = NULL,
                        transcript_backbone_colour = NULL,
                        x_translation = 0,
                        subset = NULL,
                        annotation_type = "exon",
                        breakdata = NULL,
                        species = NULL,
                        chr = NULL
                      )
                    },
                    draw_key = draw_key_polygon,
                    syn_data = function(x, layer) {
                      params <- syn_layer_params(layer)
                      context <- layer$syn_plot_context %||% NULL
                      syn_to_exon_df(
                        x = x,
                        species = params$species,
                        chr = params$chr,
                        subset = params$subset,
                        annotation_type = params$annotation_type,
                        context = context
                      )
                    },
                    syn_default_aes = c("xmin", "xmax", "ymin", "transcripts", "strand", "track", "type", "group")
)

.apply_transcript_backbone_aes <- function(data, fill = NULL, colour = NULL,
                                           use_fill_as_colour = FALSE) {
  if (nrow(data) == 0L) {
    return(data)
  }
  if (!is.null(fill)) {
    data$fill <- fill
  }
  if (!is.null(colour)) {
    data$colour <- colour
  } else if (isTRUE(use_fill_as_colour) && !is.null(fill)) {
    data$colour <- fill
  }
  data
}


#' Draw exon-style genomic features
#'
#' `geom_exon()` draws exon-like genomic intervals as filled rectangles with a
#' transcript backbone and direction indicator. It supports ordinary ggplot2
#' aesthetic mappings and can resolve `SynSpecies` / `SynIndividual` inputs
#' lazily during plot build.
#'
#' For Syn-backed layers, ggexon adds canonical identifier columns to the
#' resolved exon table so users can map aesthetics with expressions such as
#' `aes(fill = gene_id)`, `aes(fill = gene_name)`, or
#' `aes(fill = transcript_id)`.
#'
#' When `data` is a `SynSpecies`, omitting `species` uses all stored
#' individuals. Omitting `subset` keeps the full annotation table, while
#' supplying `chr` without `subset` limits the layer to that seqname.
#'
#' @param mapping Set of aesthetic mappings created by [`ggplot2::aes()`].
#'   In addition to the required positional aesthetics, Syn-backed exon layers
#'   expose canonical identifier columns `transcript_id`, `gene_id`, and
#'   `gene_name` for use in aesthetic mappings.
#' @param data A data frame, `SynSpecies`, or `SynIndividual` object.
#' @param stat,position Standard ggplot2 layer arguments.
#' @param ... Additional parameters passed on to the layer, including fixed
#'   aesthetics such as `fill`, `colour`, and `alpha`.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are removed silently.
#' @param show.legend Logical. Should this layer be included in the legend?
#' @param transcripts_track_ratio Optional transcript track ratio used by the
#'   ggexon layout helpers.
#' @param exon_height Optional exon rectangle height.
#' @param transcript_backbone_ratio Relative backbone height as a fraction of
#'   `exon_height`. Defaults to `0.1`.
#' @param transcript_arrow_ratio Relative height of the terminal transcript
#'   direction triangle as a fraction of `exon_height`. Defaults to `0.25`.
#' @param transcript_arrow_length Length of the terminal transcript direction
#'   triangle in x-axis coordinate units. Defaults to `160`.
#' @param transcript_backbone_fill,transcript_backbone_colour Optional fixed
#'   fill and outline for transcript backbones and direction indicators. When
#'   `NULL`, ggexon preserves the existing inherited/default backbone
#'   aesthetics for backwards compatibility.
#' @param x_translation Optional x offset applied before drawing.
#' @param subset Optional numeric length-2 genomic window to keep. When omitted
#'   for Syn-backed data, the full annotation range is used.
#' @param annotation_type Feature type to keep, defaults to `"exon"`.
#' @param species Optional species / individual identifier when `data` is a
#'   `SynSpecies`. When omitted, ggexon resolves exon data for all stored
#'   individuals.
#' @param chr Optional chromosome / seqname restriction when `data` is Syn-backed.
#' @param breakdata Optional break specification passed to `addbreak()`.
#' @param force_flat When `TRUE`, draw every gene on one shared y-row instead of
#'   vertically staggering overlapping genes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#'
#' @return A ggplot2 layer using [`GeomExon`].
#' @export
geom_exon <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, show.legend = NA,
                      transcripts_track_ratio = NULL, exon_height = NULL,
                      transcript_backbone_ratio = NULL,
                      transcript_arrow_ratio = NULL,
                      transcript_arrow_length = NULL,
                      transcript_backbone_fill = NULL,
                      transcript_backbone_colour = NULL,
                      x_translation = NULL, subset = NULL,
                      annotation_type ="exon",
                      species = NULL, chr = NULL,
                      breakdata = NULL,
                      force_flat = FALSE,
                      inherit.aes = TRUE) {
    params <- Filter(Negate(is.null), c(list(
      ...,
      na.rm = na.rm,
      exon_height = exon_height,
      transcript_backbone_ratio = transcript_backbone_ratio,
      transcript_arrow_ratio = transcript_arrow_ratio,
      transcript_arrow_length = transcript_arrow_length,
      transcript_backbone_fill = transcript_backbone_fill,
      transcript_backbone_colour = transcript_backbone_colour,
      x_translation = x_translation,
      subset = subset,
      annotation_type = annotation_type,
      species = species,
      chr = chr,
      breakdata = breakdata,
      force_flat = force_flat
    )))
    layer(
      data = data,
      mapping = mapping,
      geom = GeomExon,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      layer_class = LayerSyn,
      params = params)
}

GeomExon2 <- ggproto("GeomExon2", GeomExon,
  extra_params = c(
    "exon_height", "na.rm", "x_translation", "subset",
    "annotation_type", "breakdata", "species", "chr", "compress_introns",
    "intron_width", "intron_shape", "utr_height", "cds_height",
    "intron_peak", "chevron_direction", "arrow_width",
    "transcript_backbone_fill", "transcript_backbone_colour"
  ),
  default_aes = aes(
    linewidth = 0.4,
    linejoin = "mitre",
    fill = "black",
    colour = "black",
    size = 15,
    linetype = 1,
    shape = 19,
    alpha = NA,
    stroke = 1
  ),
  setup_data = function(data, params) {
    params$annotation_type <- params$annotation_type %||% "exon"
    if (identical(params$annotation_type, "all")) {
      params$annotation_type <- NULL
    }
    data <- GeomExon$setup_data(data, params)
    if (nrow(data) == 0L) {
      return(data)
    }

    .prepare_exon2_data(
      data,
      compress_introns = params$compress_introns %||% TRUE,
      intron_width = params$intron_width %||% NULL,
      intron_shape = params$intron_shape %||% "chevron",
      chevron_direction = params$chevron_direction %||% "up",
      utr_height = params$utr_height %||% 0.45,
      cds_height = params$cds_height %||% 1,
      intron_peak = params$intron_peak %||% 0.35,
      arrow_width = params$arrow_width %||% NULL
    )
  },
  draw_panel = function(data, panel_params, coord, flipped_aes = FALSE,
                        transcript_backbone_fill = NULL,
                        transcript_backbone_colour = NULL) {
    blank_flags <- if ("blank_panel" %in% names(data)) {
      isTRUE(data$blank_panel) | (data$blank_panel %in% TRUE)
    } else {
      rep(FALSE, nrow(data))
    }
    visible_data <- data[!blank_flags, , drop = FALSE]
    if (nrow(visible_data) == 0L) {
      return(zeroGrob())
    }

    intron_data <- .exon2_intron_data(
      visible_data,
      shape = unique(data$intron_shape %||% "chevron")[1L],
      direction = unique(data$chevron_direction %||% "up")[1L],
      peak = unique(data$intron_peak %||% 0.35)[1L]
    )
    arrow_data <- .exon2_arrow_data(visible_data)
    intron_data <- .apply_transcript_backbone_aes(
      intron_data,
      fill = transcript_backbone_fill,
      colour = transcript_backbone_colour,
      use_fill_as_colour = TRUE
    )
    arrow_data <- .apply_transcript_backbone_aes(
      arrow_data,
      fill = transcript_backbone_fill,
      colour = transcript_backbone_colour,
      use_fill_as_colour = TRUE
    )
    exon_data <- .exon2_trim_terminal_rects(visible_data)
    exon_grob <- ggplot2::GeomRect$draw_panel(exon_data, panel_params, coord)
    intron_grob <- if (nrow(intron_data) > 0L) {
      ggplot2::GeomPath$draw_panel(intron_data, panel_params, coord)
    } else {
      zeroGrob()
    }
    arrow_grob <- if (nrow(arrow_data) > 0L) {
      ggplot2::GeomPolygon$draw_panel(arrow_data, panel_params, coord)
    } else {
      zeroGrob()
    }

    ggname("geom_exon2", gTree(children = gList(
      intron_grob,
      exon_grob,
      arrow_grob
    )))
  },
  default_params = function() {
    list(
      exon_height = 0.8,
      x_translation = 0,
      subset = NULL,
      annotation_type = "exon",
      breakdata = NULL,
      species = NULL,
      chr = NULL,
      compress_introns = TRUE,
      intron_width = NULL,
      intron_shape = "chevron",
      chevron_direction = "up",
      utr_height = 0.45,
      cds_height = 1,
      intron_peak = 0.35,
      arrow_width = NULL,
      transcript_backbone_fill = NULL,
      transcript_backbone_colour = NULL
    )
  },
  draw_key = draw_key_polygon,
  syn_data = GeomExon$syn_data,
  syn_default_aes = GeomExon$syn_default_aes
)

#' Draw WormWeb-style exon-intron schematics
#'
#' `geom_exon2()` draws a publication-style transcript schematic inspired by
#' WormWeb exon/intron cartoons. Compared with [geom_exon()], it draws introns
#' explicitly between neighbouring feature blocks, can compress long introns,
#' and uses thinner boxes for UTR features when UTR/CDS rows are available.
#'
#' The layer keeps the same Syn-aware grammar as [geom_exon()]: it can consume a
#' data frame directly, or lazily resolve `SynIndividual` / `SynSpecies` inputs
#' during plot build.
#'
#' @param mapping Set of aesthetic mappings created by [`ggplot2::aes()`].
#' @param data A data frame, `SynSpecies`, or `SynIndividual` object.
#' @param stat,position Standard ggplot2 layer arguments.
#' @param ... Additional parameters passed on to the layer.
#' @param na.rm If `FALSE`, missing values are removed with a warning.
#' @param show.legend Logical. Should this layer be included in the legend?
#' @param transcripts_track_ratio Optional transcript track ratio used by the
#'   ggexon layout helpers.
#' @param exon_height Optional maximum exon rectangle height.
#' @param x_translation Optional x offset applied before drawing.
#' @param subset Optional numeric length-2 genomic window to keep.
#' @param annotation_type Feature type to keep. The default `"exon"` matches
#'   [geom_exon()]. Use `"all"` to keep exon, CDS, and UTR-like rows so UTR/CDS
#'   heights can differ.
#' @param species Optional species / individual identifier when `data` is a
#'   `SynSpecies`.
#' @param chr Optional chromosome / seqname restriction when `data` is Syn-backed.
#' @param breakdata Optional break specification passed to `addbreak()`.
#' @param compress_introns Logical. If `TRUE`, gaps between neighbouring feature
#'   blocks are replaced by schematic intron gaps.
#' @param intron_width Width used for compressed introns. When `NULL`, ggexon
#'   derives a width from the median feature width.
#' @param intron_shape `"chevron"` for angled WormWeb-like connectors or
#'   `"flat"` for straight intron lines.
#' @param chevron_direction Direction for chevron introns, either `"up"` or
#'   `"down"`.
#' @param utr_height Relative height for UTR-like rows.
#' @param cds_height Relative height for CDS rows. Exon rows also use this
#'   height when no explicit UTR/CDS distinction is present.
#' @param intron_peak Relative height of the chevron peak.
#' @param arrow_width Width of the terminal strand-direction triangle. When
#'   `NULL`, ggexon derives a width from the terminal exon block.
#' @param transcript_backbone_fill,transcript_backbone_colour Optional fixed
#'   fill and line/outline colour for intron connectors and terminal direction
#'   arrows. When `NULL`, ggexon preserves the existing inherited/default
#'   aesthetics for backwards compatibility.
#' @param inherit.aes If `FALSE`, overrides inherited aesthetics.
#'
#' @return A ggplot2 layer using `GeomExon2`.
#' @export
geom_exon2 <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ..., na.rm = FALSE, show.legend = NA,
                       transcripts_track_ratio = NULL,
                       exon_height = NULL, x_translation = NULL,
                       subset = NULL, annotation_type = "exon",
                       species = NULL, chr = NULL, breakdata = NULL,
                       compress_introns = TRUE, intron_width = NULL,
                       intron_shape = c("chevron", "flat"),
                       chevron_direction = c("up", "down"),
                       utr_height = 0.45, cds_height = 1,
                       intron_peak = 0.35, arrow_width = NULL,
                       transcript_backbone_fill = NULL,
                       transcript_backbone_colour = NULL,
                       inherit.aes = TRUE) {
  intron_shape <- match.arg(intron_shape)
  chevron_direction <- match.arg(chevron_direction)
  params <- Filter(Negate(is.null), c(list(
    ...,
    na.rm = na.rm,
    exon_height = exon_height,
    x_translation = x_translation,
    subset = subset,
    annotation_type = annotation_type,
    species = species,
    chr = chr,
    breakdata = breakdata,
    compress_introns = compress_introns,
    intron_width = intron_width,
    intron_shape = intron_shape,
    chevron_direction = chevron_direction,
    utr_height = utr_height,
    cds_height = cds_height,
    intron_peak = intron_peak,
    arrow_width = arrow_width,
    transcript_backbone_fill = transcript_backbone_fill,
    transcript_backbone_colour = transcript_backbone_colour
  )))
  layer(
    data = data,
    mapping = mapping,
    geom = GeomExon2,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = params
  )
}

.prepare_exon2_data <- function(data,
                                compress_introns = TRUE,
                                intron_width = NULL,
                                intron_shape = "chevron",
                                chevron_direction = "up",
                                utr_height = 0.45,
                                cds_height = 1,
                                intron_peak = 0.35,
                                arrow_width = NULL) {
  data$genomic_xmin <- data$xmin
  data$genomic_xmax <- data$xmax

  data <- data[order(data$track, data$transcripts, data$xmin, data$xmax), , drop = FALSE]
  data <- dplyr::group_by(data, .data$track, .data$transcripts)
  data <- dplyr::group_modify(data, function(.x, .y) {
    .x <- .x[order(.x$xmin, .x$xmax), , drop = FALSE]
    widths <- pmax(.x$xmax - .x$xmin, 1)
    gaps <- pmax(.x$xmin[-1L] - .x$xmax[-nrow(.x)], 0)
    gap_width <- intron_width %||% max(20, stats::median(widths, na.rm = TRUE) * 0.75)
    drawn_gaps <- if (isTRUE(compress_introns)) {
      rep(gap_width, length(gaps))
    } else {
      gaps
    }
    starts <- c(.x$xmin[1L], .x$xmin[1L] + cumsum(widths[-length(widths)] + drawn_gaps))
    .x$xmin <- starts
    .x$xmax <- starts + widths
    .x
  })
  data <- dplyr::ungroup(data)

  rel_height <- .exon2_relative_height(data$type, utr_height = utr_height, cds_height = cds_height)
  data$y_middle <- (data$ymin + data$ymax) / 2
  data$y_range <- data$y_range * rel_height
  data$ymin <- data$y_middle - data$y_range / 2
  data$ymax <- data$y_middle + data$y_range / 2
  data$intron_shape <- intron_shape
  data$chevron_direction <- chevron_direction
  data$intron_peak <- intron_peak
  data$arrow_width <- arrow_width %||% NA_real_
  data
}

.exon2_relative_height <- function(type, utr_height = 0.45, cds_height = 1) {
  type <- base::tolower(as.character(type))
  ifelse(
    grepl("utr|untranslated", type),
    utr_height,
    ifelse(type %in% c("cds", "coding"), cds_height, cds_height)
  )
}

.exon2_intron_data <- function(data, shape = "chevron", direction = "up", peak = 0.35) {
  if (nrow(data) == 0L) {
    return(data.frame())
  }

  pieces <- list()
  split_data <- split(data, interaction(data$PANEL, data$track, data$transcripts, drop = TRUE))
  id <- 1L
  for (one in split_data) {
    one <- one[order(one$xmin, one$xmax), , drop = FALSE]
    if (nrow(one) < 2L) {
      next
    }
    for (i in seq_len(nrow(one) - 1L)) {
      left <- one[i, , drop = FALSE]
      right <- one[i + 1L, , drop = FALSE]
      x1 <- left$xmax
      x2 <- right$xmin
      if (!is.finite(x1) || !is.finite(x2) || x2 <= x1) {
        next
      }
      y <- left$y_middle
      peak_sign <- if (identical(direction, "down")) -1 else 1
      y_peak <- if (identical(shape, "chevron")) y + peak_sign * left$y_range * peak else y
      pieces[[length(pieces) + 1L]] <- data.frame(
        x = c(x1, (x1 + x2) / 2, x2),
        y = c(y, y_peak, y),
        group = id,
        PANEL = left$PANEL,
        colour = left$colour %||% "black",
        linewidth = left$linewidth %||% 0.4,
        linetype = left$linetype %||% 1,
        alpha = left$alpha %||% NA_real_,
        stringsAsFactors = FALSE
      )
      id <- id + 1L
    }
  }

  if (length(pieces) == 0L) {
    return(data.frame())
  }
  dplyr::bind_rows(pieces)
}

.exon2_arrow_data <- function(data) {
  if (nrow(data) == 0L) {
    return(data.frame())
  }

  pieces <- list()
  split_data <- split(data, interaction(data$PANEL, data$track, data$transcripts, drop = TRUE))
  id <- 1L
  for (one in split_data) {
    one <- one[order(one$xmin, one$xmax), , drop = FALSE]
    if (nrow(one) == 0L) {
      next
    }

    strand <- unique(as.character(one$strand))[1L]
    if (is.na(strand) || !strand %in% c("+", "-")) {
      next
    }

    terminal <- if (identical(strand, "-")) one[1L, , drop = FALSE] else one[nrow(one), , drop = FALSE]
    width <- .exon2_arrow_width(terminal)

    if (identical(strand, "-")) {
      x <- c(terminal$xmin, terminal$xmin + width, terminal$xmin + width)
    } else {
      x <- c(terminal$xmax, terminal$xmax - width, terminal$xmax - width)
    }
    y <- c(terminal$y_middle, terminal$ymax, terminal$ymin)

    pieces[[length(pieces) + 1L]] <- data.frame(
      x = x,
      y = y,
      group = id,
      PANEL = terminal$PANEL,
      transcripts = terminal$transcripts,
      colour = terminal$colour %||% terminal$fill %||% "black",
      fill = terminal$fill %||% terminal$colour %||% "black",
      linewidth = terminal$linewidth %||% 0,
      linetype = terminal$linetype %||% 1,
      alpha = terminal$alpha %||% NA_real_,
      stringsAsFactors = FALSE
    )
    id <- id + 1L
  }

  if (length(pieces) == 0L) {
    return(data.frame())
  }
  dplyr::bind_rows(pieces)
}

.exon2_trim_terminal_rects <- function(data) {
  if (nrow(data) == 0L) {
    return(data)
  }

  out <- data
  split_indices <- split(seq_len(nrow(out)), interaction(out$PANEL, out$track, out$transcripts, drop = TRUE))
  for (idx in split_indices) {
    ordered_idx <- idx[order(out$xmin[idx], out$xmax[idx])]
    one <- out[ordered_idx, , drop = FALSE]
    strand <- unique(as.character(one$strand))[1L]
    if (is.na(strand) || !strand %in% c("+", "-")) {
      next
    }

    terminal_row <- if (identical(strand, "-")) {
      ordered_idx[1L]
    } else {
      ordered_idx[length(ordered_idx)]
    }
    terminal <- out[terminal_row, , drop = FALSE]
    width <- .exon2_arrow_width(terminal)

    if (identical(strand, "-")) {
      out$xmin[terminal_row] <- min(out$xmax[terminal_row], out$xmin[terminal_row] + width)
    } else {
      out$xmax[terminal_row] <- max(out$xmin[terminal_row], out$xmax[terminal_row] - width)
    }
  }

  out
}

.exon2_arrow_width <- function(terminal) {
  width <- terminal$arrow_width
  block_width <- max(terminal$xmax - terminal$xmin, 1)
  if (!is.finite(width) || width <= 0) {
    return(block_width * 0.35)
  }
  min(width, block_width)
}
