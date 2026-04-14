#' Plot transcript isoforms from annotation data
#'
#' `geom_transcripts()` is a transcript-focused annotation geom for plotting
#' multiple isoforms from a GTF/GFF annotation. When used with a
#' `SynIndividual` or `SynSpecies`, it can resolve exon records directly from
#' `genes=` or `transcripts=` selectors and draw one isoform per row.
GeomTranscripts <- ggproto("GeomTranscripts", GeomExon,
  required_aes = c("ymin", "xmin", "xmax", "transcripts", "strand", "track", "type"),
  non_missing_aes = c("linewidth", "shape"),
  extra_params = c(
    "exon_height", "na.rm", "x_translation", "subset", "annotation_type",
    "breakdata", "species", "chr", "genes", "transcripts"
  ),
  default_aes = aes(
    linewidth = 0,
    linejoin = "mitre",
    fill = "black",
    colour = NULL,
    size = 15,
    linetype = 1,
    shape = 19,
    alpha = NA,
    stroke = 1
  ),
  setup_data = function(data, params) {
    GeomExon$setup_data(data, params)
  },
  draw_panel = function(data, panel_params, coord, flipped_aes = FALSE) {
    if (!"transcripts" %in% names(data) && "group" %in% names(data)) {
      data$transcripts <- as.character(data$group)
    }
    GeomExon$draw_panel(data, panel_params, coord, flipped_aes = flipped_aes)
  },
  default_params = function() {
    list(
      exon_height = 0.8,
      x_translation = 0,
      subset = NULL,
      annotation_type = "exon",
      breakdata = NULL,
      species = NULL,
      chr = NULL,
      genes = NULL,
      transcripts = NULL
    )
  },
  draw_key = draw_key_polygon
)

#' @export
geom_transcripts <- function(mapping = NULL, data = NULL,
                             stat = "identity", position = "identity",
                             ..., na.rm = FALSE, show.legend = NA,
                             transcripts_track_ratio = NULL, exon_height = 0.8,
                             x_translation = 0, subset = NULL,
                             annotation_type = "exon",
                             species = NULL, chr = NULL,
                             genes = NULL, transcripts = NULL,
                             breakdata = NULL,
                             inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    geom = GeomTranscripts,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = list(
      na.rm = na.rm,
      exon_height = exon_height,
      x_translation = x_translation,
      subset = subset,
      annotation_type = annotation_type,
      species = species,
      chr = chr,
      genes = genes,
      transcripts = transcripts,
      breakdata = breakdata
    )
  )
}
