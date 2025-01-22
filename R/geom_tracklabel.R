GeomTrackLabel <- ggproto("GeomTrackLabel", Geom, 
                         required_aes = c("ymin", "xmin", "xmax", "transcripts","strand", "track", "gene_name"),
                         extra_params = c("exon_height", "na.rm", "y_scale", "x_translation"),
                         default_aes = aes(
                           colour = "black", size = 3, angle = 0, hjust = 0.5,
                           vjust = 0.5, alpha = NA, family = "sans", fontface = 1, lineheight = 1.2
                         ),
                         setup_data = function(data, params){
                           GeomExon$setup_data(data, params)
                         },
                         
                         draw_panel = function(data, panel_params, coord, check_overlap= F){
                           data2 = data %>%group_by(track) %>% mutate(gene_xmin = min(xmin), gene_xmax = max(xmax)) %>% 
                             mutate(gene_ymin = min(ymin), gene_ymax = max(ymin)) %>%
                             mutate(track_mid = (gene_ymax + 1 + gene_ymin)/2) %>% group_by(track) %>%
                             mutate(xmin = min(xmin) - 2000) %>% slice(1) %>%
                             dplyr::rename(x = xmin, y = track_mid, label = track)
                           print(data2, n =100)
                           data <- coord$transform(data2, panel_params)
                           textGrob(data$label, data$x, data$y, default.units = "native", hjust = data$hjust, 
                                    vjust = data$vjust, rot = data$angle, gp = gpar(col = alpha(data$colour, 
                                                                                                data$alpha), fontsize = data$size * .pt, fontfamily = data$family, 
                                                                                    fontface = data$fontface, lineheight = data$lineheight), 
                                    check.overlap = check_overlap)
                         }
)

geom_tracklabel <- function(mapping = NULL, data = NULL, 
                           stat = "identity", position = "identity", 
                           ..., na.rm = FALSE, show.legend = NA, y_scale = 100, 
                           x_translation = 0, exon_height = 1,
                           inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    geom = GeomTrackLabel, 
    stat = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  exon_height = exon_height,
                  y_scale = y_scale,
                  x_translation = x_translation))
}


