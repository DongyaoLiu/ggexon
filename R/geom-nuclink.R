geom_nuclink <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ...,
                                na.rm = FALSE, show.legend = NA,
                                alignment = NULL,
                                reference = NULL,
                                chr = NULL,
                                subset = NULL,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomNucLink,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = c(list(
      ...,
      na.rm = na.rm,
      alignment = alignment,
      reference = reference,
      chr = chr,
      subset = subset
    ))
  )
}

GeomNucLink <- ggproto("GeomPanel", Geom,
                             required_aes = c("tspecies", "tchr", "tstart", "tend", "strand",
                                              "qspecies", "qchr", "qstart", "qend"),
                             optional_aes = c("target_anchor_y", "query_anchor_y", "t_panel", "q_panel"),
                             non_missing_aes = c("linetype", "linewidth", "shape"),
                             extra_params = c("na.rm", "alignment", "reference", "chr", "subset"),
                             default_aes = aes(linewidth = 0,
                                               linejoin = "mitre",
                                               colour = "black",
                                               size = 15,
                                               linetype = 1,
                                               shape = 19,
                                               alpha = 0.5,
                                               stroke = 1,
                                               fill = "grey50",
                                               target_anchor_y = NA_real_,
                                               query_anchor_y = NA_real_
                             ),
                             setup_data = function(data, params) {

                               # extract the y layout information
                               link_y_out = data %>% select(PANEL, group, target_anchor_y, query_anchor_y) %>% unique() %>%
                                 melt(id = c("PANEL", "group"), variable.name = "y_variable", value.name = "y") %>%
                                 mutate(y = as.numeric(y))


                               # each row are a group will have a same id after melting
                               data$id = 1:nrow(data)
                               melt_data = data %>% select(-any_of(c("tspecies", "qspecies", "track", "tchr", "qchr", "target_anchor_y", "query_anchor_y"))) %>%
                                 melt(id = c("id", "strand", "PANEL", "group", "t_panel", "q_panel"), variable.name = "x_variable", value.name = "x") %>%
                                 mutate(y_variable = if_else(stringr::str_detect(x_variable,"^t"), "target_anchor_y", "query_anchor_y")) %>%
                                 left_join(link_y_out, join_by(PANEL == PANEL, group == group, y_variable == y_variable)) %>%
                                 mutate(source_panel = if_else(stringr::str_detect(x_variable, "^t"), t_panel, q_panel)) %>%
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
                                 arrange(id, draw_order) %>% select(-group)
                               melt_data$group = melt_data$id
                               #print(melt_data)
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
                       data <- ggplot2:::fix_linewidth(data, snake_class(self))
                       n <- nrow(data)
                       if (n == 1) return(zeroGrob())

                       munched <- .transform_link_x_by_source_panel(data, panel_params, coord) %>%
                         arrange(PANEL, group, draw_order)
                       munched = coord$transform_y(munched, panel_params[[panel]])

                       if (!"fill" %in% names(munched)) {
                         munched$fill <- "grey50"
                       }
                       if (!"linetype" %in% names(munched)) {
                         munched$linetype <- 1
                       }
                       if (!"linewidth" %in% names(munched)) {
                         munched$linewidth <- 0
                       }
                       if (!"alpha" %in% names(munched)) {
                         munched$alpha <- 0.5
                       }
                       if (!"colour" %in% names(munched)) {
                         munched$colour <- "black"
                       }

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

.transform_link_x_by_source_panel <- function(data, panel_params, coord) {
  if (!"source_panel" %in% names(data)) {
    cli::cli_abort("Link data must contain a {.field source_panel} column for x transformation.")
  }

  panel_ids <- unique(stats::na.omit(as.integer(data$source_panel)))
  if (length(panel_ids) == 0L) {
    cli::cli_abort("Link data does not define any source x panels.")
  }

  transformed <- lapply(panel_ids, function(panel_id) {
    panel_data <- data[as.integer(data$source_panel) == panel_id, , drop = FALSE]
    if (nrow(panel_data) == 0L) {
      return(NULL)
    }
    if (panel_id < 1L || panel_id > length(panel_params)) {
      cli::cli_abort("Link source panel {.val {panel_id}} is out of bounds for the current layout.")
    }

    coord$transform_x(panel_data, panel_params[[panel_id]])
  })

  transformed <- Filter(Negate(is.null), transformed)
  if (length(transformed) == 0L) {
    return(data[0, , drop = FALSE])
  }

  dplyr::bind_rows(transformed)
}
