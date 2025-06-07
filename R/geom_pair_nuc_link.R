geom_nuclink <- function(mapping = NULL, data = NULL, 
                                stat = "identity", position = "identity", 
                                ty = NULL, qy = NULL, na.rm = FALSE, show.legend = NA, 
                                inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    geom = GeomNucLink, 
    stat = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  ty = ty,
                  qy = qy)
  )
}

GeomNucLink <- ggproto("GeomPanel", Geom,
                             required_aes = c("tstart", "tend", "tstrand",
                                              "qstart", "qend", "qstrand"),
                             non_missing_aes = c("linetype", "linewidth", "shape"),
                             extra_params = c("ty","qy", "na.rm"),
                             default_aes = aes(linewidth = 0,
                                               linejoin = "mitre", colour = "black",
                                               size = 15,
                                               linetype = 1,
                                               shape = 19,
                                               alpha = 0.5,
                                               stroke = 1,
                                               fill = "grey50", 
                             ),
                             setup_data = function(data, params) {
                               data$id = 1:nrow(data)
                               melt_data = data %>% select(-PANEL, -group) %>% 
                                 melt(id = c("id", "qstrand", "tstrand"), variable.name = "x_variable") %>%
                                 rename(x = value)
                               
                               print(melt_data)
                               data2 = melt_data %>% mutate(y = if_else(str_detect(x_variable,"^t"),
                                                                        params$ty, params$qy)) %>%
                                 mutate(align_direction = if_else(qstrand == tstrand,
                                                                  "same", "opposite")) %>%
                                 arrange(id, x_variable) %>% rowwise() %>%
                                 mutate(draw_order = 
                                          case_when(align_direction == "same" && x_variable == "tstart" ~ 1,
                                                    align_direction == "same" && x_variable == "tend" ~ 2,
                                                    align_direction == "same" && x_variable == "qstart" ~ 4,
                                                    align_direction == "same" && x_variable == "qend" ~ 3,
                                                    align_direction == "opposite" && x_variable == "tstart" ~ 1,
                                                    align_direction == "opposite" && x_variable == "tend" ~ 2,
                                                    align_direction == "opposite" && x_variable == "qstart" ~ 3,
                                                    align_direction == "opposite" && x_variable == "qend" ~ 4)) %>%
                                 arrange(id, draw_order) %>% rename(group = id) %>%
                                 select(group, x, y, draw_order) %>%
                               mutate(
                                   PANEL = 1, colour = GeomNucLink$default_aes$colour,
                                   linejoin = GeomNucLink$default_aes$linejoin,
                                   linewidth = GeomNucLink$default_aes$linewidth,
                                   alpha = GeomNucLink$default_aes$alpha,
                                 )
                               
                               data2
                             }
                             ,
                             
                             draw_panel = function(data, panel_params, coord){
                               GeomPolygon$draw_panel(data, panel_params, coord)
                             }
)



ggplot(data = track_data) + geom_pairwise_panel(aes(xmin = start, xmax = end, 
                                                    track = track), sandwich_ratio = c(1,2,3)) + 
  geom_nuclink(data = nuclink_data, aes(tstart = tstart, tend = tend, tstrand = tstrand,
                                        qstart = qstart, qend = qend, qstrand = qstrand), 
               sandwich_ratio = c(1,2,3)) + 
  theme_void()

nuclink_data = data.frame(tstart = c(3000,10000),
        tend = c(8000, 18000),
        tstrand = c("+", "+"),
        qstart = c(5000, 8000),
        qend = c(7000, 10000),
        qstrand = c("+", "-"))

