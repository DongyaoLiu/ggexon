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
                            fontface = 1, lineheight = 1.2
                         ),
                         default_params = function() {
                           list(
                             exon_height = 0.4,
                             x_translation = 0,
                             species = NULL,
                             chr = NULL,
                             subset = NULL
                           )
                         },
                         setup_data = function(data, params){
                           GeomExon$setup_data(data, params)
                         },

                         draw_panel = function(data, panel_params, coord, check_overlap= F){
                           data2 = data %>%group_by(transcripts) %>% mutate(gene_xmin = min(xmin), gene_xmax = max(xmax))  %>%
                             mutate(gene_ymax = ymax , x_mid = (gene_xmax + gene_xmin)/2) %>% dplyr::slice(1) %>%
                             dplyr::rename(x = x_mid, y = gene_ymax)
                           #print(data2, n =100)
                           data <- coord$transform(data2, panel_params)
                           textGrob(data$label, data$x, data$y, default.units = "native", hjust = data$hjust,
                                    vjust = data$vjust, rot = data$angle, gp = gpar(col = alpha(data$colour,
                                                                                                data$alpha), fontsize = data$size * .pt, fontfamily = data$family,
                                                                                    fontface = data$fontface, lineheight = data$lineheight),
                                    check.overlap = check_overlap)
                         }
)

#' Draw gene labels on exon tracks
#'
#' `geom_genelabel()` places one text label per transcript or gene span on an
#' exon-style genomic track. It uses the same Syn-backed lazy data resolution as
#' [`geom_exon()`], so labels can be drawn from `SynIndividual` or `SynSpecies`
#' containers as well as from precomputed data frames.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param x_translation Optional x offset applied before drawing.
#' @param exon_height Optional exon rectangle height used when preparing track
#'   coordinates.
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
                       species = NULL, chr = NULL, subset = NULL,
                       inherit.aes = TRUE) {
    params <- Filter(Negate(is.null), c(list(
      ...,
      na.rm = na.rm,
      exon_height = exon_height,
      x_translation = x_translation,
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
