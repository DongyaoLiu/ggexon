GeomTrack <- ggproto("GeomTrack", Geom, 
                     required_aes = c("ymin", "xmin", "xmax", "transcripts","strand", "track"),
                     extra_params = c("exon_height", "na.rm", "y_scale", "x_translation","show.lenged"),
                     default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),
                     
                     setup_data = function(data, params){
                       GeomExon$setup_data(data, params)
                     },
                     draw_panel = function(data, panel_params, coord){
                       track_data = add_transcripts_seq_line(data)
                       GeomSegment$draw_panel(track_data, panel_params, coord, arrow = NULL,
                                              lineend = "butt", linejoin = "round", na.rm = FALSE)
                     }
)


