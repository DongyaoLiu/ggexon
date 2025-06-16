create_layout2 <- function(facet, coord, layout = NULL) {
   layout <- layout %||% Layout
   ggplot2:::check_inherits(layout, "Layout2")
   ggproto(NULL, layout, facet = facet, coord = coord)
}

#' @export
Layout2 <- ggproto("Layout2", Layout,
  map_link_position = function(self, link_data, link_data_name) {
     # map the position of linkage data, should only used in facet-genomics.
     # transform the link data to long dataframe.
     # print(layout)

    layout = self$layout
    y_range = self$panel_scales_y[[1]]$range$range


    target = str_split_1(link_data_name, "_")[1]
    query = str_split_1(link_data_name, "_")[2]
    ROW_Query = layout[which(query == layout$track), "ROW"]
    ROW_Target = layout[which(target == layout$track), "ROW"]
    Query_PANEL = as.numeric(layout[which(query == layout$track), "PANEL"])
    Target_PANEL = as.numeric(layout[which(target == layout$track), "PANEL"])

    x_panel_range_list = purrr::map(self$panel_params, ~ .x$x$scale$get_limits())
    names(x_panel_range_list) = layout$PANEL
    print(x_panel_range_list)
    y_panel_range = self$panel_params[[1]]$y$scale$range$range

    if (ROW_Query > ROW_Target){
      Query_y = max(y_range)
      Target_y = min(y_range)
    }else{
      Query_y = min(y_range)
      Target_y = max(y_range)
    }

    apply_rescale <- function(x, panel, scales_list) {
        panel <- as.character(panel)
        params <- scales_list[[panel]]

    if (is.null(params)) {
        warning(paste("No scale parameters for PANEL", panel))
        return(x)
    }
        #print(x)
        #print(params)
        rescaled = rescale(x, c(0, 1), params)
    }


    link_data$id = 1:nrow(link_data)
    melt_data = link_data %>% select(id, TStart, TEnd, QStart, QEnd, Strand) %>%
        melt(id = c("id", "Strand"), variable.name = "x_variable", value.name = "x")
    data2 = melt_data %>% mutate(y = if_else(str_detect(x_variable,"^T"),
                                             Target_y, Query_y)) %>%
      mutate(align_direction = if_else(Strand == "+",
                                       "same", "opposite")) %>%
      arrange(id, x_variable) %>% rowwise() %>%
      mutate(draw_order =
               case_when(align_direction == "same" && x_variable == "TStart" ~ 1,
                         align_direction == "same" && x_variable == "TEnd" ~ 2,
                         align_direction == "same" && x_variable == "QStart" ~ 4,
                         align_direction == "same" && x_variable == "QEnd" ~ 3,
                         align_direction == "opposite" && x_variable == "TStart" ~ 1,
                         align_direction == "opposite" && x_variable == "TEnd" ~ 2,
                         align_direction == "opposite" && x_variable == "QStart" ~ 3,
                         align_direction == "opposite" && x_variable == "QEnd" ~ 4)) %>%
      arrange(id, draw_order) %>%
      mutate(PANEL = if_else(str_detect(x_variable, "^T"), Target_PANEL, Query_PANEL)) %>%
      select(id ,draw_order, x, y, PANEL) %>% group_by(id) %>%
      filter(!any(if_any(c(x, y), is.na))) %>% group_by(PANEL) %>%
      mutate(x_scaled = apply_rescale(x, dplyr::first(PANEL), x_panel_range_list)) %>% group_by(id) %>%
      filter(all(x_scaled <= 1)) %>% ungroup()

      return(data2)
   }
)
