#' Geom implementation for amino-acid variant lollipops
#'
#' Internal ggproto used by [`geom_aa_variant()`]. Draws each variant as a head
#' (point) raised above the exon row on a short stem, so amino-acid variants can
#' be read directly against the exon/intron model.
GeomAaVariant <- ggproto("GeomAaVariant", Geom,
  required_aes = c("x", "y"),
  optional_aes = "label",
  non_missing_aes = c("size", "shape", "colour"),
  extra_params = c(
    "na.rm", "species", "chr", "subset", "annotation", "genes", "transcripts",
    "strains", "mutation", "event_type", "min_sample_count", "protein_ranges",
    "ref", "exon_height", "stem_height", "y_offset", "stem", "stem_colour",
    "stem_linewidth", "spread", "spread_threshold", "curve_k", "label_size",
    "label_angle", "label_gap", "label_colour"
  ),
  default_aes = aes(
    colour = "firebrick",
    fill = "firebrick",
    size = 2.5,
    shape = 21,
    alpha = NA,
    stroke = 0.5,
    linewidth = 0.4,
    linetype = 1
  ),
  setup_data = function(data, params) {
    stem_height <- params$stem_height %||% 0.6
    # Plain data-frame layers may not provide a stem base; drop the head onto a
    # default stem so the geom still renders without Syn projection.
    if (!"y_base" %in% names(data)) {
      data$y_base <- data$y - stem_height
    }
    data
  },
  draw_panel = function(data, panel_params, coord, stem = TRUE,
                        spread = TRUE, spread_threshold = NULL, curve_k = 1.2,
                        stem_colour = "grey45", stem_linewidth = 0.4,
                        label_size = 2, label_angle = 90, label_gap = 0.12,
                        label_colour = "grey20") {
    if (nrow(data) == 0L) {
      return(zeroGrob())
    }
    if (!"y_base" %in% names(data)) {
      data$y_base <- data$y
    }

    # Spread heads horizontally so dots/labels do not overlap; the stem base
    # stays anchored at the true codon position and a sigmoid stem links them.
    x_head <- data$x
    if (isTRUE(spread) && nrow(data) > 1L) {
      thr <- spread_threshold %||% (diff(range(data$x, na.rm = TRUE)) / 50)
      if (is.finite(thr) && thr > 0) {
        x_head <- .spread_lollipop_positions(data$x, thr)
      }
    }

    grobs <- list()
    if (isTRUE(stem)) {
      stems <- .generate_lollipop_stems(
        x = data$x, y = data$y_base, xend = x_head, yend = data$y,
        n = 30, k = curve_k
      )
      stem_df <- data.frame(
        x = stems$lollipop_x, y = stems$lollipop_y,
        group = stems$lollipop_curve_id,
        colour = stem_colour, linewidth = stem_linewidth,
        linetype = 1, alpha = NA, stringsAsFactors = FALSE
      )
      grobs[[length(grobs) + 1L]] <- ggplot2::GeomPath$draw_panel(
        stem_df, panel_params, coord
      )
    }

    heads <- data
    heads$x <- x_head
    grobs[[length(grobs) + 1L]] <- ggplot2::GeomPoint$draw_panel(
      heads, panel_params, coord
    )

    if ("label" %in% names(data)) {
      lab <- data
      lab$x <- x_head
      lab$y <- data$y + label_gap
      lab$colour <- label_colour
      lab$size <- label_size
      lab$angle <- label_angle
      lab$hjust <- 0
      lab$vjust <- 0.5
      lab$family <- ""
      lab$fontface <- 1
      lab$lineheight <- 0.9
      lab$alpha <- NA
      grobs[[length(grobs) + 1L]] <- ggplot2::GeomText$draw_panel(
        lab, panel_params, coord
      )
    }

    ggname("geom_aa_variant", gTree(children = do.call(gList, grobs)))
  },
  default_params = function() {
    list(
      species = NULL,
      chr = NULL,
      subset = NULL,
      annotation = NULL,
      genes = NULL,
      transcripts = NULL,
      strains = NULL,
      mutation = NULL,
      event_type = NULL,
      min_sample_count = NULL,
      protein_ranges = NULL,
      ref = NULL,
      exon_height = 0.8,
      stem_height = 0.6,
      y_offset = 0,
      stem = TRUE,
      spread = TRUE,
      spread_threshold = NULL,
      curve_k = 1.2,
      stem_colour = "grey45",
      stem_linewidth = 0.4,
      label_size = 2,
      label_angle = 90,
      label_gap = 0.12,
      label_colour = "grey20"
    )
  },
  draw_key = draw_key_point,
  syn_data = function(x, layer) {
    params <- syn_layer_params(layer)
    context <- layer$syn_plot_context %||% NULL
    syn_to_aa_variant_df(
      x = x,
      species = params$species,
      chr = params$chr,
      subset = params$subset,
      annotation = params$annotation,
      genes = params$genes,
      transcripts = params$transcripts,
      strains = params$strains,
      mutation = params$mutation,
      event_type = params$event_type,
      min_sample_count = params$min_sample_count,
      protein_ranges = params$protein_ranges,
      ref = params$ref,
      exon_height = params$exon_height %||% 0.8,
      stem_height = params$stem_height %||% 0.6,
      y_offset = params$y_offset %||% 0,
      context = context
    )
  },
  syn_default_aes = c("x", "y", "y_base", "track", "transcripts", "group")
)

#' Annotate amino-acid variants on the exon structure
#'
#' `geom_aa_variant()` draws protein-coordinate variants (for example `C316H`
#' at residue 316) at their genomic codon positions, as lollipop markers sitting
#' above the exon/intron model produced by [geom_exon()]. When the plot data is
#' a `SynIndividual` or `SynSpecies`, the layer resolves an attached
#' `SynProteinMutationAnnotation`, projects each variant onto its transcript's
#' CDS structure with [project_mutations_to_genome()] (splice-aware, with codons
#' that cross an intron mapped to the exonic base), and aligns each marker with
#' the matching transcript row of the exon layer.
#'
#' The resolved table exposes the variant metadata columns (such as `position`,
#' `ref`, `alt`, `mutation`, and `sample_count`) so they can drive aesthetics,
#' e.g. `aes(fill = sample_count)` or `aes(colour = ref)`.
#'
#' A plain data frame with `x`/`y` columns can also be supplied for full manual
#' control.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments.
#' @param species Optional species/individual selector when plotting from a
#'   `SynSpecies` object.
#' @param chr Optional chromosome name used to define the projection window.
#' @param subset Optional genomic window used to limit projected variants.
#' @param annotation Optional `SynProteinMutationAnnotation` layer name.
#' @param genes,transcripts Optional identifiers limiting the transcripts that
#'   variants are projected onto.
#' @param strains,mutation,event_type,min_sample_count,protein_ranges,ref
#'   Optional variant filters forwarded to [query_protein_mutations()].
#' @param exon_height Exon rectangle height used to align markers with the exon
#'   layer. Defaults to `0.8` to match [geom_exon()].
#' @param stem_height Height of the lollipop stem above the exon top.
#' @param y_offset Additional vertical offset applied to every marker.
#' @param stem Logical; draw the connecting stem under each head.
#' @param stem_colour,stem_linewidth Stem styling.
#' @param spread Logical; when `TRUE` (default) heads are pushed apart
#'   horizontally so crowded lollipops do not overlap, while each stem stays
#'   anchored at the true codon position.
#' @param spread_threshold Minimum horizontal gap (in genomic units) enforced
#'   between adjacent heads. When `NULL`, a value scaled to the data range is
#'   used.
#' @param curve_k Sigmoid steepness of the curved stems linking each anchored
#'   base to its (possibly spread) head.
#' @param label_size,label_angle,label_gap,label_colour Styling for optional
#'   text labels. Labels are drawn at the spread head positions when a `label`
#'   aesthetic is mapped, e.g. `aes(label = mutation)`.
#'
#' @return A ggplot2 layer using [`GeomAaVariant`].
#' @seealso [project_mutations_to_genome()], [geom_exon()], [geom_motif()]
#' @export
geom_aa_variant <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ..., na.rm = FALSE, show.legend = NA,
                            species = NULL, chr = NULL, subset = NULL,
                            annotation = NULL, genes = NULL, transcripts = NULL,
                            strains = NULL, mutation = NULL, event_type = NULL,
                            min_sample_count = NULL, protein_ranges = NULL,
                            ref = NULL, exon_height = NULL, stem_height = NULL,
                            y_offset = NULL, stem = NULL, spread = NULL,
                            spread_threshold = NULL, curve_k = NULL,
                            stem_colour = NULL, stem_linewidth = NULL,
                            label_size = NULL, label_angle = NULL,
                            label_gap = NULL, label_colour = NULL,
                            inherit.aes = TRUE) {
  params <- Filter(Negate(is.null), c(list(
    ...,
    na.rm = na.rm,
    species = species,
    chr = chr,
    subset = subset,
    annotation = annotation,
    genes = genes,
    transcripts = transcripts,
    strains = strains,
    mutation = mutation,
    event_type = event_type,
    min_sample_count = min_sample_count,
    protein_ranges = protein_ranges,
    ref = ref,
    exon_height = exon_height,
    stem_height = stem_height,
    y_offset = y_offset,
    stem = stem,
    spread = spread,
    spread_threshold = spread_threshold,
    curve_k = curve_k,
    label_size = label_size,
    label_angle = label_angle,
    label_gap = label_gap,
    label_colour = label_colour,
    stem_colour = stem_colour,
    stem_linewidth = stem_linewidth
  )))
  layer(
    data = data,
    mapping = mapping,
    geom = GeomAaVariant,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    layer_class = LayerSyn,
    params = params
  )
}

# Resolve Syn data into a per-variant table positioned on the exon track.
syn_to_aa_variant_df <- function(x,
                                 species = NULL,
                                 chr = NULL,
                                 subset = NULL,
                                 annotation = NULL,
                                 genes = NULL,
                                 transcripts = NULL,
                                 strains = NULL,
                                 mutation = NULL,
                                 event_type = NULL,
                                 min_sample_count = NULL,
                                 protein_ranges = NULL,
                                 ref = NULL,
                                 exon_height = 0.8,
                                 stem_height = 0.6,
                                 y_offset = 0,
                                 context = NULL) {
  species <- resolve_context_species_params(x, species, context)

  if (methods::is(x, "SynSpecies") && length(species %||% character()) > 1L) {
    species <- unique(as.character(species))
    return(dplyr::bind_rows(lapply(species, function(species_name) {
      syn_to_aa_variant_df(
        x = x, species = species_name, chr = chr, subset = subset,
        annotation = annotation, genes = genes, transcripts = transcripts,
        strains = strains, mutation = mutation, event_type = event_type,
        min_sample_count = min_sample_count, protein_ranges = protein_ranges,
        ref = ref, exon_height = exon_height, stem_height = stem_height,
        y_offset = y_offset, context = context
      )
    })))
  }

  individual <- resolve_syn_individual(x, species = species)
  ann <- resolve_syn_protein_mutation_annotation(
    individual, annotation = annotation, allow_missing = TRUE
  )
  if (is.null(ann)) {
    return(data.frame())
  }

  window <- normalize_syn_window_request(
    x = x,
    species = syn_id(individual),
    chr = chr,
    subset = subset,
    allow_missing_subset = TRUE,
    context = context,
    geom = "geom_aa_variant"
  )

  proj <- project_mutations_to_genome(
    x = individual,
    annotation = annotation,
    genes = genes,
    transcripts = transcripts,
    strains = strains,
    mutation = mutation,
    event_type = event_type,
    min_sample_count = min_sample_count,
    protein_ranges = protein_ranges,
    ref = ref,
    chr = window$chr,
    start = window$start,
    end = window$end
  )
  if (nrow(proj) == 0L) {
    return(data.frame())
  }

  # Collapse the (possibly intron-split) codon segments of each variant to a
  # single marker placed on the widest exonic fragment, so the head always
  # lands on coding sequence rather than inside an intron.
  vkey <- paste(proj$transcripts, proj$position, proj$mutation, sep = "\r")
  collapsed <- lapply(split(proj, vkey), function(rows) {
    pick <- which.max(rows$xmax - rows$xmin)
    row <- rows[pick, , drop = FALSE]
    row$x <- (row$xmin + row$xmax) / 2
    row
  })
  collapsed <- dplyr::bind_rows(collapsed)

  # Align each marker with the matching transcript row of the exon layer by
  # reusing the exon table's per-transcript ymin.
  exon_df <- syn_to_exon_df(
    x = x, species = species, chr = chr, subset = subset,
    annotation_type = "exon", context = context
  )
  if (is.data.frame(exon_df) && nrow(exon_df) > 0L &&
      all(c("transcripts", "ymin") %in% names(exon_df))) {
    ymap <- exon_df[!duplicated(exon_df$transcripts), c("transcripts", "ymin"), drop = FALSE]
    base_ymin <- ymap$ymin[match(collapsed$transcripts, ymap$transcripts)]
  } else {
    base_ymin <- rep(NA_real_, nrow(collapsed))
  }
  base_ymin[is.na(base_ymin)] <- 2

  exon_top <- base_ymin + exon_height
  out <- collapsed
  out$y_base <- exon_top
  out$y <- exon_top + stem_height + y_offset
  out$track <- syn_id(individual)
  out$group <- seq_len(nrow(out))
  out$PANEL <- 1L
  rownames(out) <- NULL
  out
}
