#‘ construct basic geom element to represents the gene.
#' construct the function of to generate the 

GeomGeneTag <- ggproto("GeomGeneTag", Geom,
                    required_aes = c("gene", "trackname", "xmin", "xmax", "track_y", "strand"),
                    non_missing_aes = c("linewidth", "shape"),
                    extra_params = c("tag_height", "tag_width", "tag_angle", "tag_rotate_angle", "na.rm", "position2"),
                    default_aes = aes(linewidth = 0, fill = "black", linejoin = "mitre", 
                                      colour = "black",
                                      size = 15,
                                      linetype = 1,
                                      shape = 19,
                                      alpha = NA,
                                      stroke = 1
                    ),
                    
                    setup_data = function(data, params){
                      # trackname_levels = tibble(levels = levels(data$trackname), track_y = 1:length(levels(data$trackname)))
                      # data$track_y = data[ ,which(data$trackname %in% trackname_levels$name)]
                      data = data %>% mutate(x = (xmin + xmax)/2) %>% 
                        group_by(trackname) %>% mutate(track_length = max(xmax) - min(xmin)) %>% ungroup()
                      print(data)
                      tag_data = add_genetag(data, tag_height=params$tag_height, 
                                             tag_width=params$tag_width, 
                                             tag_angle=params$tag_angle, 
                                             tag_rotate_angle=params$tag_rotate_angle, 
                                             point_number = 4)
                      if (params$position2 == "center") {
                        #' need one more or two parameter to control distance between the tags. 
                        #' or automatically compute form the gene number and track length. 
                        print("position of tags: center")
                        
                      }else if(params$position2 == "identity"){
                        print("position of tags: identity")
                      }else if(params$position2 == "cluster"){
                        print("position of tags: cluster")
                      }
                      return(data)
                     
                    },
                    
                    draw_panel = function(data, panel_params, coord, flipped_aes = FALSE){

                      ggname("geom_genetag", gTree(children = gList(
                        GeomPolygon$draw_panel(tri_data, panel_params, coord)
                          )
                        )
                      )
                    },
                    draw_key = draw_key_polygon
)

geom_genetag <- function(mapping = NULL, data = NULL, 
                       stat = "identity", position = "identity", 
                       ..., na.rm = FALSE, show.legend = TRUE, 
                       tag_height = NULL,
                       tag_width = NULL, 
                       tag_angle = NULL,
                       tag_rotate_angle = NULL,
                       position2 = NULL,
                       inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    geom = GeomGeneTag, 
    stat = stat, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, 
                  tag_height = tag_height,
                  tag_width = tag_width, 
                  tag_angle = tag_angle,
                  tag_rotate_angle = tag_rotate_angle,
                  position2 = position2))
}
