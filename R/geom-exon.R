#' v=1.0
#' author = "LIU Dongyao"
#' LabWebsite: "www.zhenglabhku.org"
#' E-mail = "dongyao@connect.hku.hk"
#' @param break_list named list, used for the specify the break region.

GeomExon <- ggproto("GeomExon", Geom,
                      required_aes = c("ymin", "xmin", "xmax", "transcripts","strand", "track", "type"),
                      non_missing_aes = c("linewidth", "shape"),
                      extra_params = c("exon_height", "na.rm", "y_scale", "x_translation", "subset", "annotation_type",
                                       "breakdata", "species", "chr"),
                      default_aes = aes(linewidth = 0, linejoin = "mitre", fill="black",
                        colour = NULL,
                        size = 15,
                        linetype = 1,
                        shape = 19,
                        alpha = NA,
                        stroke = 1
                      ),

                    setup_data = function(data, params){
                      x_translation <- if (is.null(params$x_translation)) 0 else params$x_translation
                      exon_height <- if (is.null(params$exon_height)) 0.8 else params$exon_height
                      y_scale <- if (is.null(params$y_scale)) 100 else params$y_scale

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



                      if (x_translation != 0){
                        data = data %>% mutate(xmin = xmin + x_translation, xmax =xmax + x_translation)
                      }

                      rec_data = seq_add_y(data = data,
                                           track_proportion = params$transcripts_track_ratio,
                                           y_scale = y_scale,
                                           exon_proportion = 0.8, blank_proportion = 0.2,
                                           sandwich_ratio = params$sandwich_ratio,
                                           exon_height = exon_height)


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
                    default_params = function() {
                      list(
                        exon_height = 0.8,
                        y_scale = 100,
                        x_translation = 0,
                        subset = NULL,
                        annotation_type = "exon",
                        breakdata = NULL,
                        species = NULL,
                        chr = NULL
                      )
                    },
                    draw_key = draw_key_polygon
)



geom_exon <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, show.legend = NA,
                      transcripts_track_ratio = NULL, y_scale = NULL, exon_height = NULL,
                      x_translation = NULL, subset = NULL,
                      annotation_type ="exon",
                      species = NULL, chr = NULL,
                      breakdata = NULL,
                      inherit.aes = TRUE) {
    params <- Filter(Negate(is.null), list(
      na.rm = na.rm,
      exon_height = exon_height,
      y_scale = y_scale,
      x_translation = x_translation,
      subset = subset,
      annotation_type = annotation_type,
      species = species,
      chr = chr,
      breakdata = breakdata
    ))
    layer(
      data = data,
      mapping = mapping,
      geom = GeomExon,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      layer_class = LayerSyn,
      params = params)
}
