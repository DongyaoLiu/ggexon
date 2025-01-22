geom_track_backgroud <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                na.rm = FALSE, show.legend = NA, x_buffer = c(1000,1000), exon_height = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomTrackbBackgroud,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  exon_height = exon_height,
                  x_buffer = x_buffer)
  )
}

GeomTrackbBackgroud <- ggproto("GeomRect", Geom,
                             required_aes = c("ymin", "track", "start", "end"),
                             non_missing_aes = c("linetype", "linewidth", "shape"),
                             extra_params = c("na.rm", "exon_height", "x_buffer"),
                             default_aes = aes(linewidth = 0,
                                               linejoin = "mitre", fill = "grey",
                                               size = 15,
                                               linetype = 1,
                                               shape = 19,
                                               alpha = 1,
                                               stroke = 1
                             ),


                             setup_data = function(data, params) {
                               #xmin = min(data$start) - params$x_buffer[1]
                               xmin = 0 - params$x_buffer[1]
                               xmax = max(abs(data$end - data$start)) + params$x_buffer[2]
                               data = data %>% group_by(track) %>% mutate(xmax = max(end) - min(start)) %>% drop_na(ymin) %>% arrange(ymin) %>% group_by(track) %>%
                                 mutate(ymax = params$exon_height + max(ymin)+2) %>%
                                 mutate(ymin = min(ymin)) %>% slice(1) %>% ungroup() %>%
                                 mutate(xmin = xmin, track_order = dense_rank(ymin)) %>%
                                 mutate(fill = if_else(track_order %% 2 == 0,  "green", "blue"))
                               print(data)

                                 #
                                 #mutate(fill = if_else(track_order %% 2 == 0,  "green", "blue"))
                               ##fill
                              print(data)

                             }
                             ,

                             draw_panel = function(data, panel_params, coord){
                               coords <- coord$transform(data, panel_params)
                               rectGrob(
                                 coords$xmin, coords$ymax,
                                 width = coords$xmax - coords$xmin,
                                 height = coords$ymax - coords$ymin,
                                 default.units = "native",
                                 just = c("left", "top"),
                                 gp = gpar(
                                   col = coords$colour,
                                   fill = alpha(coords$fill, coords$alpha),
                                   lwd = coords$linewidth * .pt,
                                   lty = coords$linetype,
                                   linejoin = coords$linejoin,
                                   lineend = coords$lineend
                                 )
                               )
                             }
)



