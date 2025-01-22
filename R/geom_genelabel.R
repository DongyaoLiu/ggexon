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
                            fontface = 1, lineheight = 1.2
                         ),
                         setup_data = function(data, params){
                           GeomExon$setup_data(data, params)
                         },

                         draw_panel = function(data, panel_params, coord, check_overlap= F){
                           data2 = data %>%group_by(transcripts) %>% mutate(gene_xmin = min(xmin), gene_xmax = max(xmax))  %>%
                             mutate(gene_ymax = ymax , x_mid = (gene_xmax + gene_xmin)/2) %>% slice(1) %>%
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

geom_genelabel <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity", x_translation = 0,
                       ..., na.rm = FALSE, show.legend = NA, exon_height = 0.4,
                       inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      geom = GeomGeneLabel,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    exon_height = exon_height,
                    x_translation= x_translation))
}



