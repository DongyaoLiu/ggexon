GeomGeneLabel <- ggproto("GeomGeneLabel", Geom,
                         required_aes = c("ymin", "xmin", "xmax", "transcripts","strand", "track", "label"),
                         non_missing_aes = "angle",
                         default_aes = aes(
                           colour = "black",
                           family = "sans",
                           size = 3,
                           angle = 0, hjust = 0,
                           vjust = 0.5, alpha = NA, fontface = 1, lineheight = 1.2
                         ),
                         extra_params = c("exon_height", "na.rm", "x_translation",
                            "species", "chr", "subset",
                            "label_direction", "label_offset_fraction",
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
                             label_offset_fraction = 0.3
                           )
                         },
                         setup_data = function(data, params){
                           GeomExon$setup_data(data, params)
                         },

                         draw_panel = function(data, panel_params, coord, check_overlap = FALSE,
                                                exon_height = 0.8,
                                                label_direction = "top",
                                                label_offset_fraction = 0.3){
                           label_direction <- match.arg(label_direction, c("top", "bottom"))
                           actual_exon_height <- max(data$ymax - data$ymin, na.rm = TRUE)
                           if (actual_exon_height <= 0) actual_exon_height <- exon_height
                           label_offset <- actual_exon_height * label_offset_fraction

                           genomic_range <- diff(range(c(data$xmin, data$xmax), na.rm = TRUE))
                           if (genomic_range <= 0) genomic_range <- 1

                           data2 <- data %>%
                             group_by(transcripts) %>%
                             mutate(
                               gene_xmin = min(xmin),
                               gene_xmax = max(xmax),
                               orig_x_mid = (gene_xmax + gene_xmin) / 2,
                               gene_ymax = ymax,
                               gene_ymin = ymin
                             ) %>%
                             dplyr::slice(1) %>%
                             ungroup() %>%
                             arrange(orig_x_mid)

                           # estimate text width in data coordinates
                           # size is in mm; each char ≈ 0.5 × size mm; panel ≈ 150 mm
                           data2 <- data2 %>%
                             mutate(
                               est_nchar = nchar(as.character(label)),
                               est_width = est_nchar * size * genomic_range / 600,
                               label_x  = orig_x_mid
                             )

                           # greedy horizontal slide to prevent overlap
                           min_gap <- genomic_range * 0.005
                           if (nrow(data2) > 1) {
                             for (i in 2:nrow(data2)) {
                               prev_right <- data2$label_x[i - 1] +
                                 data2$est_width[i - 1] / 2 + min_gap
                               curr_left <- data2$label_x[i] -
                                 data2$est_width[i] / 2
                               if (curr_left < prev_right) {
                                 data2$label_x[i] <- prev_right +
                                   data2$est_width[i] / 2
                               }
                             }
                           }

                           # per-gene label y and leader line anchor
                           if (label_direction == "top") {
                             data2$anchor_y <- data2$gene_ymax
                             data2$label_y <- data2$gene_ymax + label_offset
                           } else {
                             data2$anchor_y <- data2$gene_ymin
                             data2$label_y <- data2$gene_ymin - label_offset
                           }

                           # prepare label positions for coord transform
                           label_data <- data2
                           label_data$x <- label_data$label_x
                           label_data$y <- label_data$label_y
                           label_t <- coord$transform(label_data, panel_params)

                           # prepare leader line endpoints
                           leader_start <- data2
                           leader_start$x <- leader_start$orig_x_mid
                           leader_start$y <- leader_start$anchor_y
                           leader_start_t <- coord$transform(leader_start, panel_params)

                           leader_end <- data2
                           leader_end$x <- leader_end$label_x
                           leader_end$y <- leader_end$label_y
                           leader_end_t <- coord$transform(leader_end, panel_params)

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

                           lg <- segmentsGrob(
                             x0 = leader_start_t$x, y0 = leader_start_t$y,
                             x1 = leader_end_t$x,   y1 = leader_end_t$y,
                             default.units = "native",
                             gp = gpar(col = "grey60", lwd = 0.5)
                           )

                           gList(lg, tg)
                         }
)

#' Draw gene labels on exon tracks
#'
#' `geom_genelabel()` places one text label per transcript or gene span on an
#' exon-style genomic track. Labels sit on a single horizontal line above or
#' below the exon tracks, with leader lines connecting each label to its gene
#' body. Overlapping labels are pushed apart horizontally.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param x_translation Optional x offset applied before drawing.
#' @param exon_height Optional exon rectangle height used when preparing track
#'   coordinates.
#' @param label_direction Where to place the label line: `"top"` (above the
#'   highest track) or `"bottom"` (below the lowest track). Default `"top"`.
#' @param label_offset_fraction Distance between the exon tracks and the label
#'   line, expressed as a fraction of `exon_height`. Default `0.3`.
#' @param species Optional species / individual identifier when `data` is a
#'   `SynSpecies`.
#' @param chr Optional chromosome / seqname restriction when `data` is
#'   Syn-backed.
#' @param subset Optional numeric length-2 genomic window to keep.
#'
#' @return A ggplot2 layer using the internal `GeomGeneLabel` ggproto.
#' @export
geom_genelabel <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity", x_translation = NULL,
                       ..., na.rm = FALSE, show.legend = NA, exon_height = NULL,
                       label_direction = NULL, label_offset_fraction = NULL,
                       species = NULL, chr = NULL, subset = NULL,
                       inherit.aes = TRUE) {
    params <- Filter(Negate(is.null), c(list(
      ...,
      na.rm = na.rm,
      exon_height = exon_height,
      x_translation = x_translation,
      label_direction = label_direction,
      label_offset_fraction = label_offset_fraction,
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
      params = params)
}
