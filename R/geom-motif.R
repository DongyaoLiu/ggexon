GeomMotif <- ggproto("GeomMotif", Geom,
                    required_aes = c("ymin", "xmin", "xmax", "transcripts", "strand", "track", "text"),
                    extra_params = c("exon_height", "na.rm", "y_scale", "x_translation"),
                    default_aes = aes(linewidth = 0, linejoin = "mitre", fill="black",
                                      colour = NULL,
                                      size = 15,
                                      linetype = 1,
                                      shape = 19,
                                      alpha = NA,
                                      stroke = 1
                    ),

                    setup_data = function(data, params){
                      GeomExon$setup_data(data, params)
                    },

                    draw_panel = function(data, panel_params, coord, flipped_aes = FALSE){
                      track_data = add_transcripts_seq_line(data)
                      track_data$linewidth = 2
                      transcripts_line_Grob = GeomSegment$draw_panel(track_data, panel_params, coord)
                      tri_data = add_transcripts_direction(track_data)
                      tri_data$linewidth = 0
                      transcripts_tri_Grob = GeomPolygon$draw_panel(tri_data, panel_params, coord)
                      ggname("geom_exon", gTree(children = gList(
                        transcripts_line_Grob,
                        GeomRect$draw_panel(data, panel_params, coord),
                        transcripts_tri_Grob
                      )
                      )
                      )
                    },
                    draw_key = draw_key_polygon
)
