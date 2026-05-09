#' v=1.0
#' author = "LIU Dongyao"
#' LabWebsite: "www.zhenglabhku.org"
#' E-mail = "dongyao@connect.hku.hk"
#' Description: designed to draw gene without exon information. An arrow.


GeomGene <- ggproto("GeomGene", Geom,
                    required_aes = c("ymin", "xmin", "xmax", "transcripts","strand", "track"),
                    non_missing_aes = c("linewidth", "shape"),
                    extra_params = c("exon_height", "na.rm", "x_translation", "proportion_trim3",
                                     "species", "chr", "subset"),
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

                      if (x_translation != 0){
                        data = data %>% mutate(xmin = xmin + x_translation, xmax =xmax + x_translation)
                      }
                      data = data %>% group_by(track) %>%
                        mutate(x_adjustment = 0) %>%
                        group_by(transcripts) %>% mutate(xmin = min(xmin),
                                                             xmax = max(xmax), transcripts_length = abs(xmax - xmin))  %>% dplyr::slice(1) %>%
                        mutate(Xmax = xmax,
                               Xmin = xmin,
                               xmin,xmax = if_else(strand == "+", xmax - transcripts_length * params$proportion_trim3, xmax),
                               xmin = if_else(strand == "-", xmin + transcripts_length * params$proportion_trim3, xmin))
                      rec_data = seq_add_y(data = data,
                                           track_proportion = params$transcripts_track_ratio,
                                           exon_proportion = 0.8, blank_proportion = 0.2,
                                           sandwich_ratio = params$sandwich_ratio,
                                           exon_height = exon_height)
                    },

                    draw_panel = function(data, panel_params, coord, flipped_aes = FALSE){
                      #track_data = add_transcripts_seq_line(data, transcripts_stop_length = NULL, transcripts_stop_proportion)
                      track_data = data %>% mutate(xend = if_else(strand == "+", xmax, xmin),
                                                   a_x = if_else(strand == "+", Xmax, Xmin),
                                                   a_y = y_middle,
                                                   yend = y_middle,
                                                   linewidth = 0,
                                                   linetype = if ("linetype" %in% names(data)) linetype else 1,
                                                   fill = if ("fill" %in% names(data)) fill else "black",
                                                   colour = if ("colour" %in% names(data) && !all(is.na(colour))) colour else fill)
                      tri_data = add_transcripts_direction(track_data, ratio = 1, angle = pi/3, lengthABS = NULL, lengthPRO = NULL)
                      transcripts_tri_Grob = GeomPolygon$draw_panel(tri_data, panel_params, coord)
                      ggname("geom_exon", gTree(children = gList(
                        GeomRect$draw_panel(data, panel_params, coord),
                        transcripts_tri_Grob
                      )
                      )
                      )
                    },
                    default_params = function() {
                      list(
                        exon_height = 0.8,
                        x_translation = 0,
                        proportion_trim3 = 0.2,
                        species = NULL,
                        chr = NULL,
                        subset = NULL
                      )
                    },
                    draw_key = draw_key_polygon,
                    syn_data = function(x, layer) {
                      params <- syn_layer_params(layer)
                      context <- layer$syn_plot_context %||% NULL
                      syn_to_gene_df(
                        x = x,
                        species = params$species,
                        chr = params$chr,
                        subset = params$subset,
                        context = context
                      )
                    },
                    syn_default_aes = c("xmin", "xmax", "ymin", "transcripts", "strand", "track", "group")
)


geom_gene <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ..., na.rm = FALSE, show.legend = NA,
                      transcripts_track_ratio = NULL, exon_height = NULL,
                      x_translation = NULL, proportion_trim3 = 0.2,
                      species = NULL, chr = NULL, subset = NULL,
                      inherit.aes = TRUE) {
  params <- Filter(Negate(is.null), c(list(
    ...,
    na.rm = na.rm,
    exon_height = exon_height,
    x_translation = x_translation,
    proportion_trim3 = proportion_trim3,
    species = species,
    chr = chr,
    subset = subset
  )))
  layer(
    data = data,
    mapping = mapping,
    geom = GeomGene,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = params)
}
