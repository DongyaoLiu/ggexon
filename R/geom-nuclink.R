geom_nuclink <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomNucLink,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm)
  )
}

GeomNucLink <- ggproto("GeomPanel", Geom,
                             required_aes = c("tspecies", "tchr", "tstart", "tend", "strand",
                                              "qspecies", "qchr", "qstart", "qend"),
                             non_missing_aes = c("linetype", "linewidth", "shape"),
                             extra_params = c("na.rm"),
                             default_aes = aes(linewidth = 0,
                                               linejoin = "mitre",
                                               colour = "black",
                                               size = 15,
                                               linetype = 1,
                                               shape = 19,
                                               alpha = 0.5,
                                               stroke = 1,
                                               fill = "grey50",
                             ),
                             setup_data = function(data, params) {

                               # extract the y layout information
                               link_y_out = data %>% select(PANEL, group, ty, qy) %>% unique() %>%
                                 melt(id = c("PANEL", "group"), variable.name = "y_variable", value.name = "y") %>%
                                 mutate(y = as.numeric(y))


                               # each row are a group will have a same id after melting
                               data$id = 1:nrow(data)
                               melt_data = data %>% select(,-tspecies, -qspecies, -track, -tchr, -qchr, -ty, -qy) %>%
                                 melt(id = c("id", "strand", "PANEL", "group"), variable.name = "x_variable", value.name = "x") %>%
                                 mutate(y_variable = if_else(str_detect(x_variable,"^t"), "ty", "qy")) %>%
                                 left_join(link_y_out, join_by(PANEL == PANEL, group == group, y_variable == y_variable)) %>%
                                 arrange(id, x_variable) %>% rowwise() %>%
                                 mutate(draw_order =
                                          case_when(strand == "+" && x_variable == "tstart" ~ 1,
                                                    strand == "+" && x_variable == "tend" ~ 2,
                                                    strand == "+" && x_variable == "qstart" ~ 4,
                                                    strand == "+" && x_variable == "qend" ~ 3,
                                                    strand == "-" && x_variable == "tstart" ~ 1,
                                                    strand == "-" && x_variable == "tend" ~ 2,
                                                    strand == "-" && x_variable == "qstart" ~ 3,
                                                    strand == "-" && x_variable == "qend" ~ 4)) %>%
                                 arrange(id, draw_order) %>% select(-group) %>% rename(group = id)
                               print(melt_data)
                               melt_data
                             },

                         draw_layer = function(self, data, params, layout, coord) {
                         if (empty(data)) {
                           n <- if (is.factor(data$PANEL)) nlevels(data$PANEL) else 1L
                           return(rep(list(zeroGrob()), n))
                         }

                         # Trim off extra parameters
                         params <- params[intersect(names(params), self$parameters())]

                         if (nlevels(as.factor(data$PANEL)) > 1L) {
                           data_panels <- split(data, data$PANEL)
                         } else {
                           data_panels <- list(data)
                         }


                         lapply(data_panels, function(data) {
                           if (empty(data)) return(zeroGrob())
                           panel = data$PANEL[1]
                           inject(self$draw_panel(data, layout$panel_params, coord, panel, !!!params))
                         })
                       },
                       draw_panel = function(data, panel_params, coord, panel, rule = "evenodd",
                                             lineend = "butt", linejoin = "round", linemitre = 10){
                       upper_panel = as.numeric(panel) - 1
                       upper_panel_params = panel_params[[upper_panel]]
                       print(upper_panel_params)

                       lower_panel = as.numeric(panel) + 1
                       lower_panel_params = panel_params[[lower_panel]]


                       upper_range = upper_panel_params$x$scale$range$range
                       lower_range = lower_panel_params$x$scale$range$range



                       data <- ggplot2:::fix_linewidth(data, snake_class(self))
                       n <- nrow(data)
                       if (n == 1) return(zeroGrob())

                       data_split = split(data, data$y_variable)
                       if (unique(data_split[["ty"]]$y) > unique(data_split[["qy"]]$y)) {

                         munched_t <- coord$transform_x(data_split[["ty"]], upper_panel_params)
                         munched_q <- coord$transform_x(data_split[["qy"]], lower_panel_params)
                       }else{

                         munched_t <- coord$transform_x(data_split[["ty"]], lower_panel_params)
                         munched_q <- coord$transform_x(data_split[["qy"]], upper_panel_params)
                       }
                       munched = rbind(munched_t, munched_q) %>% arrange(PANEL, group, draw_order)
                       munched = coord$transform_y(munched, panel_params[[panel]])

                       first_idx <- !duplicated(munched$group)
                       first_rows <- munched[first_idx, ]

                       ggname(
                         "geom_link_polygon",
                         polygonGrob(
                           munched$x, munched$y, default.units = "native",
                           id = munched$group,
                           gp = gg_par(
                             col = first_rows$colour,
                             fill = fill_alpha(first_rows$fill, first_rows$alpha),
                             lwd = first_rows$linewidth,
                             lty = first_rows$linetype,
                             lineend = lineend,
                             linejoin = linejoin,
                             linemitre = linemitre
                           )
                         )
                       )
                       }
)
