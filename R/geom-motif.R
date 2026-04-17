GeomMotif <- ggproto("GeomMotif", Geom,
                    required_aes = c("ymin", "xmin", "xmax", "transcripts", "strand", "track", "text"),
                    extra_params = c(
                      "exon_height", "na.rm", "y_scale", "x_translation",
                      "species", "chr", "subset", "annotation", "ids", "domains",
                      "model", "motif", "y_offset"
                    ),
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
                      ggname("geom-motif", gTree(children = gList(
                        GeomRect$draw_panel(data, panel_params, coord)
                        )
                      )
                      )
                    },
                    default_params = function() {
                      list(
                        exon_height = 0.8,
                        y_scale = 100,
                        x_translation = 0,
                        species = NULL,
                        chr = NULL,
                        subset = NULL,
                        annotation = NULL,
                        ids = NULL,
                        domains = NULL,
                        model = "all",
                        motif = NULL,
                        y_offset = 0
                      )
                    },
                    draw_key = draw_key_polygon
)

#' Plot protein-domain motifs
#'
#' When the plot data is a `SynIndividual` or `SynSpecies`, this layer resolves
#' an attached `SynProteinDomainAnnotation` and draws domain intervals as motif
#' blocks along protein-coordinate tracks.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param exon_height Height of the motif blocks.
#' @param y_scale Vertical spacing between protein tracks.
#' @param x_translation Optional x-axis offset.
#' @param species Optional species/individual selector when plotting from a
#'   `SynSpecies` object.
#' @param chr Optional chromosome name used to define the genomic projection
#'   window.
#' @param subset Optional genomic window used to limit the projected motifs.
#' @param annotation Optional annotation-layer name. Defaults to the first
#'   attached `SynProteinDomainAnnotation`.
#' @param ids Optional identifiers to match against the protein-domain
#'   annotation key column.
#' @param domains Optional domain names/accessions to filter.
#' @param model InterProScan analysis model(s) to display. Accepts a single
#'   string, a character vector, or `"all"`. When multiple models are supplied,
#'   their order is used from top to bottom.
#' @param motif Optional motif name(s) used to filter the InterProScan table.
#' @param y_offset Vertical offset applied to the motif band so protein domains
#'   can be separated from exon rectangles on the same genomic track.
#'
#' @return A ggplot layer.
#' @export
geom_motif <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ..., na.rm = FALSE, show.legend = NA,
                       exon_height = NULL, y_scale = NULL, x_translation = NULL,
                       species = NULL, chr = NULL, subset = NULL,
                       annotation = NULL, ids = NULL, domains = NULL,
                       model = "all", motif = NULL,
                       y_offset = NULL,
                       inherit.aes = TRUE) {
  params <- Filter(Negate(is.null), list(
    na.rm = na.rm,
    exon_height = exon_height,
    y_scale = y_scale,
    x_translation = x_translation,
    species = species,
    chr = chr,
    subset = subset,
    annotation = annotation,
    ids = ids,
    domains = domains,
    model = model,
    motif = motif,
    y_offset = y_offset
  ))
  layer(
    data = data,
    mapping = mapping,
    geom = GeomMotif,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = params
  )
}
