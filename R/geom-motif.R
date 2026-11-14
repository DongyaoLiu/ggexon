GeomMotif <- ggproto("GeomMotif", Geom,
                    ggexon_panel_role = "annotation",
                    required_aes = c("ymin", "xmin", "xmax", "transcripts", "strand", "track", "text"),
                    extra_params = c(
                      "exon_height", "na.rm", "x_translation",
                      "species", "chr", "subset", "annotation", "ids", "domains",
                      "model", "motif", "y_offset", "genomic_layout"
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
                      if (!is.null(params$genomic_layout)) return(GeomRect$setup_data(data, params))
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
                    draw_key = draw_key_polygon,
                    syn_data = function(x, layer) {
                      params <- syn_layer_params(layer)
                      context <- layer$syn_plot_context %||% NULL
                      if (!is.null(params$genomic_layout)) return(.prepared_motif_layer_data(x, layer, context))
                      syn_to_motif_df(
                        x = x,
                        species = params$species,
                        chr = params$chr,
                        subset = params$subset,
                        annotation = params$annotation,
                        ids = params$ids,
                        domains = params$domains,
                        model = params$model %||% "all",
                        motif = params$motif,
                        y_offset = params$y_offset %||% 0,
                        context = context
                      )
                    },
                    syn_default_aes = c("xmin", "xmax", "ymin", "ymax", "transcripts", "strand", "track", "text", "group")
)

.check_prepared_motif_mapping <- function(mapping) {
  if (any(c("xmin", "xmax", "ymin", "ymax") %in% names(mapping))) {
    stop("Prepared motif coordinates cannot be changed with a position aesthetic mapping.", call. = FALSE)
  }
}

.prepared_motif_layer_data <- function(x, layer, context = NULL) {
  params <- syn_layer_params(layer)
  individuals <- .prepared_layout_individuals(x, params, context)
  pieces <- lapply(individuals, function(individual) {
    entry <- .get_genomic_layout(individual, params$genomic_layout)
    annotation <- params$annotation
    ann <- if (!is.null(annotation) && annotation %in% annotation_names(individual)) {
      get_annotation(individual, annotation)
    } else NULL
    if (!methods::is(ann, "SynGenomicProjectionAnnotation") || !identical(ann@feature_kind, "domain")) {
      stop("Prepared motifs require an explicit stored domain projection in `annotation`.", call. = FALSE)
    }
    height <- entry$parameters$track_ymax - entry$parameters$track_ymin
    explicit <- layer$geom_params$exon_height
    if (!is.null(explicit) && !isTRUE(all.equal(as.numeric(explicit), height))) {
      stop("`exon_height` conflicts with the prepared genomic layout.", call. = FALSE)
    }
    out <- project_domains_to_genome(individual, annotation = annotation,
      model = params$model %||% "all", ids = params$ids,
      domains = params$domains, motif = params$motif)
    if (nrow(out)) {
      out <- out[out$transcripts %in% entry$transcripts & out$seqnames %in% entry$chr, , drop = FALSE]
    }
    # Stored projections retain base centres; only drawing intervals gain edges.
    edge <- if (isTRUE(entry$parameters$base_edges)) 0.5 else 0
    out$xmin <- as.numeric(out$xmin) - edge
    out$xmax <- as.numeric(out$xmax) + edge
    out$ymin <- rep(entry$parameters$track_ymin, nrow(out))
    out$ymax <- rep(entry$parameters$track_ymax, nrow(out))
    out$track <- rep(syn_id(individual), nrow(out))
    out$individual <- out$track
    out$group <- seq_len(nrow(out))
    out
  })
  out <- dplyr::bind_rows(pieces)
  attr(out, "ggexon_genomic_layout") <- params$genomic_layout
  out
}

#' Plot protein-domain motifs
#'
#' When the plot data is a `SynIndividual` or `SynSpecies`, this layer resolves
#' an attached `SynProteinDomainAnnotation` and draws domain intervals as motif
#' blocks along protein-coordinate tracks.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param exon_height Height of the motif blocks.
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
#' @param genomic_layout Optional prepared genomic layout name. Requires an
#'   explicitly named stored domain projection in `annotation`; segments use
#'   the cached transcript, chromosome, exon height, and base-edge convention.
#'
#' @return A ggplot layer.
#' @export
geom_motif <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ..., na.rm = FALSE, show.legend = NA,
                       exon_height = NULL, x_translation = NULL,
                       species = NULL, chr = NULL, subset = NULL,
                       annotation = NULL, ids = NULL, domains = NULL,
                       model = "all", motif = NULL,
                       y_offset = NULL,
                       inherit.aes = TRUE, genomic_layout = NULL) {
  layer_class <- LayerSyn
  if (!is.null(genomic_layout)) {
    .genomic_name(genomic_layout, "genomic_layout")
    .prepared_geometry_arguments(match.call(), c("y_offset", "x_translation", "subset", "chr"))
    if (!identical(stat, "identity") && !inherits(stat, "StatIdentity")) {
      stop("Prepared motifs require `stat = \"identity\"`.", call. = FALSE)
    }
    if (!identical(position, "identity") && !inherits(position, "PositionIdentity")) {
      stop("Prepared motifs require `position = \"identity\"`.", call. = FALSE)
    }
    .check_prepared_motif_mapping(mapping)
    .check_prepared_motif_mapping(list(...))
    if (!is.null(data) && !methods::is(data, "SynIndividual") && !methods::is(data, "SynSpecies")) {
      stop("A prepared genomic layout requires Syn object input.", call. = FALSE)
    }
    layer_class <- ggproto(NULL, LayerSyn,
      setup_layer = function(self, data, plot) {
        if (isTRUE(self$inherit.aes)) .check_prepared_motif_mapping(plot@mapping)
        ggproto_parent(LayerSyn, self)$setup_layer(data, plot)
      })
  }
  params <- Filter(Negate(is.null), c(list(...), list(
    na.rm = na.rm,
    genomic_layout = genomic_layout,
    exon_height = exon_height,
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
  )))
  layer(
    data = data,
    mapping = mapping,
    geom = GeomMotif,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = layer_class,
    params = params
  )
}
