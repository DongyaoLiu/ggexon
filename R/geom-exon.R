#' v=1.0
#' author = "LIU Dongyao"
#' LabWebsite: "www.zhenglabhku.org"
#' E-mail = "dongyao@connect.hku.hk"
#' @param break_list named list, used for the specify the break region.

GeomExon <- ggproto("GeomExon", Geom,
                      required_aes = c("ymin", "xmin", "xmax", "transcripts","strand", "track", "type"),
                      non_missing_aes = c("linewidth", "shape"),
                      extra_params = c("exon_height", "na.rm", "x_translation", "subset", "annotation_type",
                                       "breakdata"),
                      default_aes = aes(linewidth = 0, linejoin = "mitre", fill="black",
                        colour = NULL,
                        size = 15,
                        linetype = 1,
                        shape = 19,
                        alpha = NA,
                        stroke = 1
                      ),

                    setup_data = function(data, params){
                      if (!is.null(params$annotation_type)){
                        data = data %>% filter(type == params$annotation_type)
                      }
                      if (!is.null(params$subset)) {
                        #' filter base on the subset region.
                        start1 = int(params$subset[1])
                        end1 = int(params$subset[2])
                        data = data %>% filter(xmin >= start1, xmax <= end1)
                      }
                      if (!is.null(params$breakdata)){
                        data = addbreak(data, params$breakdata)
                      }

                      data = data %>% group_by(track) %>%
                        mutate(x_adjustment = 0)



                      if (params$x_translation!=0){
                        data = data %>% mutate(xmin = xmin + params$x_translation, xmax =xmax + params$x_translation)
                      }

                      rec_data = seq_add_y(data = data,
                                           track_proportion = params$transcripts_track_ratio,
                                           y_scale = params$y_scale,
                                           exon_proportion = 0.8, blank_proportion = 0.2,
                                           sandwich_ratio = params$sandwich_ratio,
                                           exon_height = params$exon_height)


                    },

                      draw_panel = function(data, panel_params, coord, flipped_aes = FALSE){
                        track_data = add_transcripts_seq_line(data)
                        track_data$linewidth = 1
                        transcripts_line_Grob = GeomSegment$draw_panel(track_data, panel_params, coord)
                        tri_data = add_transcripts_direction(track_data)
                        tri_data$linewidth = 0
                        transcripts_tri_Grob = GeomPolygon$draw_panel(tri_data, panel_params, coord)
                        #print(getAnywhere("GeomRect"))
                        exon_Grob = ggplot2::GeomRect$draw_panel(data, panel_params, coord)
                        ggname("geom_exon", gTree(children = gList(
                          transcripts_line_Grob,
                          exon_Grob,
                          transcripts_tri_Grob
                            )
                          )
                        )
                      },
                    draw_key = draw_key_polygon
)



geom_exon <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, show.legend = NA,
                      transcripts_track_ratio = NULL, exon_height=0.8,
                      x_translation = 0, subset = NULL,
                      annotation_type ="exon",
                      breakdata = NULL,
                      inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      geom = GeomExon,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    exon_height = exon_height,
                    x_translation = x_translation,
                    subset = subset,
                    annotation_type = annotation_type,
                    breakdata = breakdata))
}


