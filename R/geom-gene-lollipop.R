#' Draw labelled gene arrows on stems at true genomic positions
#'
#' Fixed-size arrow heads make selected genes readable over large genomic
#' ranges. Their stems retain the mapped genomic positions. Heads are packed
#' above a shared baseline, in genomic order, using physical millimetres.
#'
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   ggplot2 layer arguments. Map `x`, `y`, `strand` and optionally `label`.
#'   Columns named `x`, `y`, and `strand` are mapped automatically.
#' @param headroom Positive y-data-unit space reserved above the baseline.
#' @param head_width_mm Common physical head width. `NULL` fits the longest
#'   label across the entire layer without shrinking text.
#' @param head_height_mm Minimum head height in millimetres; grows to fit text.
#' @param padding_mm Padding around internal labels in millimetres.
#' @param tip_mm Length of the triangular arrow tip in millimetres.
#' @param gap_mm Horizontal clearance between heads in the same row.
#' @param row_gap_mm Vertical clearance between rows of heads.
#' @param stem_height_mm Clearance from the baseline to the first head row.
#' @param max_shift_mm Maximum horizontal displacement from a true anchor.
#' @param max_rows Maximum number of head rows, or `Inf`.
#' @param head_layout `"compact"` retains ordered centres with interleaved rows.
#'   `"columns"` groups consecutive genes into columns read top to bottom,
#'   then left to right, and routes leaders beside the heads.
#' @param anchor_tick_mm Height of exact-position vertical anchor ticks.
#' @param fan_height_mm Height of the clear routing band above the ticks in
#'   column mode. `stem_height_mm` separates this band from the lowest heads.
#' @param lane_spacing_mm Distance between leader lanes in each column gutter.
#' @param lane_clearance_mm Clearance beside gutter lanes and head outlines.
#' @param stem_linewidth Width of coloured leaders in millimetres. Their colour
#'   defaults to a darker shade of the head fill; map or set `stem_colour` to
#'   override it. Anchor ticks use at most 0.16 mm linewidth.
#' @param stem_halo_mm Extra halo width on each side of a leader, drawn beneath
#'   all coloured leaders.
#' @param stem_halo_colour Halo colour; defaults to white.
#' @param layout_callback Optional function receiving a data frame of physical
#'   head and anchor coordinates at draw time, once per panel per draw. Units
#'   are millimetres relative to the panel's lower left corner. `genomic_x`
#'   retains the original untransformed genomic coordinate.
#' @param species,chr,subset Optional Syn individual, chromosome and genomic
#'   window selectors.
#'
#' @details
#' Use one shared `y` baseline per panel. The common width includes arrow-tip
#' space so labels fit inside the arrow body. Missing labels remain unlabelled
#' heads. Unknown strands produce rectangular heads. Reversing the x scale or
#' coordinate reverses the arrows. Only linear Cartesian coordinates are
#' supported. Heads outside a coordinate zoom are omitted with their anchors.
#'
#' Heads are stylised gene symbols, not scaled transcript structures. For
#' Syn-backed input the anchor is the midpoint of the complete gene span,
#' including genes without coding transcripts. Retain native-scale context
#' genes in a separate `geom_gene()` layer if needed.
#'
#' Compact packing uses the smallest feasible number of round-robin rows with ordered
#' head centres and bounded displacement. Automatic column packing uses the smallest
#' feasible number of rows with one common x centre per group of consecutive
#' genes. Read each column top to bottom, then the next column to its right.
#' Map optional numeric `head_row` to keep matching genes in shared rows across
#' facets. Rows are positive integers numbered bottom to top. Supply a row for
#' every gene in the layer, and use `head_layout = "columns"`. Fixed-row packing
#' finds the smallest feasible number of ordered, contiguous columns while
#' retaining every supplied row index. Unused slots stay
#' blank; duplicated genes remain separate heads. Shared rows are supplied
#' layout metadata, not inferred orthology or evidence of gene absence.
#'
#' Leaders run through a clear band and ordered lanes to each head's left edge,
#' avoiding the other heads. Both layouts are recomputed on the output device.
#' If every head cannot fit the available width or headroom, drawing errors
#' instead of dropping genes or reducing font size. Increase the output size,
#' headroom or displacement allowance to resolve the error.
#'
#' @examples
#' genes <- data.frame(x = c(1e6, 1.02e6, 2e6), y = 0,
#'                     strand = c("+", "-", "+"), label = c("A", "B*", "C"))
#' ggplot2::ggplot() + geom_gene_lollipop(data = genes) +
#'   ggplot2::scale_x_continuous(limits = c(0, 6e6))
#'
#' @return A ggplot layer.
#' @export
geom_gene_lollipop <- function(mapping = NULL, data = NULL, stat = "identity",
    position = "identity", ..., headroom = 1, head_width_mm = NULL,
    head_height_mm = 4, padding_mm = .6, tip_mm = 1.8, gap_mm = .8,
    row_gap_mm = .8, stem_height_mm = 1.5, max_shift_mm = 25,
    max_rows = Inf, head_layout = c("compact", "columns"),
    anchor_tick_mm = .9, fan_height_mm = 2.5, lane_spacing_mm = .65,
    lane_clearance_mm = .5, stem_linewidth = .22, stem_halo_mm = .12,
    stem_halo_colour = "white", layout_callback = NULL, species = NULL, chr = NULL,
    subset = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {
  head_layout <- match.arg(head_layout)
  exprs <- if (is.null(mapping)) list() else as.list(mapping)
  for (col in if (isTRUE(inherit.aes)) character() else c("x", "y", "strand")) {
    if (is.null(exprs[[col]])) exprs[[col]] <- rlang::sym(col)
  }
  if (!isTRUE(inherit.aes) && (is.data.frame(data) || methods::is(data, "DataFrame"))) {
    for (col in intersect(c("label", "track", "individual", "species", "gene_id",
                           "gene_name", "genomic_x", "genomic_xmin",
                           "genomic_xmax", "strip_x_direction", "head_row"), colnames(data))) {
      if (is.null(exprs[[col]])) exprs[[col]] <- rlang::sym(col)
    }
  }
  mapping <- rlang::inject(ggplot2::aes(!!!exprs))
  if (is.null(mapping$genomic_x)) mapping$genomic_x <- mapping$x
  # Resolve effective inherited mappings before scales transform x. This also
  # lets Syn's compiler install its mappings before genomic provenance is copied.
  layer_class <- ggproto(NULL, LayerSyn,
    setup_layer = function(self, data, plot) {
      data <- ggplot2::ggproto_parent(LayerSyn, self)$setup_layer(data, plot)
      fallback <- intersect(c("x", "y", "strand", "label", "track", "individual",
        "species", "gene_id", "gene_name", "genomic_x", "genomic_xmin",
        "genomic_xmax", "strip_x_direction", "head_row"), names(data))
      for (col in fallback) {
        if (is.null(self$computed_mapping[[col]]))
          self$computed_mapping[[col]] <- rlang::new_quosure(rlang::sym(col))
      }
      if (is.null(self$computed_mapping$genomic_x))
        self$computed_mapping$genomic_x <- self$computed_mapping$x
      data
    })
  layer(data = data, mapping = mapping, geom = GeomGeneLollipop, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    layer_class = layer_class, params = Filter(Negate(is.null), list(..., headroom = headroom,
      head_width_mm = head_width_mm, head_height_mm = head_height_mm,
      padding_mm = padding_mm, tip_mm = tip_mm, gap_mm = gap_mm,
      row_gap_mm = row_gap_mm, stem_height_mm = stem_height_mm,
      max_shift_mm = max_shift_mm, max_rows = max_rows,
      head_layout = head_layout, anchor_tick_mm = anchor_tick_mm,
      fan_height_mm = fan_height_mm, lane_spacing_mm = lane_spacing_mm,
      lane_clearance_mm = lane_clearance_mm, stem_linewidth = stem_linewidth,
      stem_halo_mm = stem_halo_mm, stem_halo_colour = stem_halo_colour,
      layout_callback = layout_callback, species = species, chr = chr,
      subset = subset, na.rm = na.rm)))
}

#' Gene-lollipop geom
#' @rdname geom_gene_lollipop
#' @export
GeomGeneLollipop <- ggproto("GeomGeneLollipop", Geom,
  ggexon_panel_role = "annotation",
  required_aes = c("x", "y", "strand"),
  default_aes = aes(colour = "grey35", fill = "grey75", linewidth = .25,
    linetype = 1, alpha = NA_real_, head_row = NA_real_, stem_colour = NA_character_, label = "", size = 3,
    family = "", fontface = 1, lineheight = 1.2, label_colour = NA_character_,
    track = NA_character_, individual = NA_character_, species = NA_character_,
    gene_id = NA_character_, gene_name = NA_character_, genomic_x = NA_real_,
    genomic_xmin = NA_real_, genomic_xmax = NA_real_, strip_x_direction = 1),
  extra_params = c("na.rm", "headroom", "head_width_mm", "head_height_mm",
    "padding_mm", "tip_mm", "gap_mm", "row_gap_mm", "stem_height_mm",
    "max_shift_mm", "max_rows", "head_layout", "anchor_tick_mm", "fan_height_mm",
    "lane_spacing_mm", "lane_clearance_mm", "stem_linewidth", "stem_halo_mm",
    "stem_halo_colour", "layout_callback", "species", "chr", "subset"),
  setup_data = function(data, params) {
    data$ymin <- data$y
    data$ymax <- data$y + .genebox_positive_number(params$headroom %||% 1, "headroom")
    data
  },
  handle_na = function(data, params) {
    bad <- !is.finite(data$x) | !is.finite(data$y)
    if (any(bad) && !isTRUE(params$na.rm))
      warning("Removed ", sum(bad), " row(s) with missing positions in geom_gene_lollipop().", call. = FALSE)
    data[!bad, , drop = FALSE]
  },
  draw_layer = function(self, data, params, layout, coord) {
    # Validate the whole layer so an entirely unassigned facet cannot silently
    # switch to automatic rows beside facets using a shared fixed template.
    if (any(!is.na(data$head_row)) &&
        (!is.numeric(data$head_row) || any(!is.finite(data$head_row)) ||
         any(data$head_row < 1 | data$head_row != floor(data$head_row))))
      stop("`head_row` must supply a positive integer for every gene in the layer.", call. = FALSE)
    params$global_label_metrics <- data
    ggplot2::ggproto_parent(ggplot2::Geom, self)$draw_layer(data, params, layout, coord)
  },
  draw_panel = function(data, panel_params, coord, headroom = 1,
      head_width_mm = NULL, head_height_mm = 4, padding_mm = .6,
      tip_mm = 1.8, gap_mm = .8, row_gap_mm = .8, stem_height_mm = 1.5,
      max_shift_mm = 25, max_rows = Inf, head_layout = "compact",
      anchor_tick_mm = .9, fan_height_mm = 2.5, lane_spacing_mm = .65,
      lane_clearance_mm = .5, stem_linewidth = .22, stem_halo_mm = .12,
      stem_halo_colour = "white", layout_callback = NULL,
      global_label_metrics = NULL) {
    if (!nrow(data)) return(zeroGrob())
    if (!isTRUE(coord$is_linear()) || inherits(coord, "CoordFlip"))
      stop("geom_gene_lollipop() requires linear Cartesian coordinates.", call. = FALSE)
    coords <- coord$transform(data, panel_params)
    keep <- is.finite(coords$x) & is.finite(coords$y) &
      coords$x >= 0 & coords$x <= 1 & coords$y >= 0 & coords$y <= 1
    data <- data[keep, , drop = FALSE]
    coords <- coords[keep, , drop = FALSE]
    if (!nrow(data)) return(zeroGrob())
    if (length(unique(data$y)) != 1L)
      stop("Use one gene-lollipop baseline per panel; facet separate baselines.", call. = FALSE)
    upper <- data
    upper$y <- data$y + headroom
    top <- coord$transform(upper, panel_params)$y
    if (any(top <= coords$y))
      stop("Gene-lollipop headroom must extend upwards on the plotted y axis.", call. = FALSE)
    s <- as.character(data$strand)
    direction <- ifelse(is.na(s), 0, ifelse(s == "+", 1, ifelse(s == "-", -1, 0))) *
      .genebox_x_orientation(panel_params, coord) *
      .genebox_strip_x_direction(data$strip_x_direction)
    grid::gTree(cl = "geneLollipopGrob", data = data, coords = coords,
      upper = top, direction = direction,
      metrics = global_label_metrics %||% data,
      settings = list(head_width_mm = head_width_mm, head_height_mm = head_height_mm,
        padding_mm = padding_mm, tip_mm = tip_mm, gap_mm = gap_mm,
        row_gap_mm = row_gap_mm, stem_height_mm = stem_height_mm,
        max_shift_mm = max_shift_mm, max_rows = max_rows,
        head_layout = head_layout, anchor_tick_mm = anchor_tick_mm,
        fan_height_mm = fan_height_mm, lane_spacing_mm = lane_spacing_mm,
        lane_clearance_mm = lane_clearance_mm, stem_linewidth = stem_linewidth,
        stem_halo_mm = stem_halo_mm, stem_halo_colour = stem_halo_colour,
        layout_callback = layout_callback))
  },
  draw_key = ggplot2::draw_key_polygon,
  syn_data = function(x, layer) {
    p <- syn_layer_params(layer)
    d <- syn_to_gene_df(x, species = p$species, chr = p$chr, subset = p$subset,
                        context = layer$syn_plot_context %||% NULL)
    if (nrow(d)) {
      d$x <- (d$xmin + d$xmax) / 2
      d$y <- d$ymin
      d$genomic_x <- d$x
      d$genomic_xmin <- d$xmin
      d$genomic_xmax <- d$xmax
    }
    d
  },
  syn_default_aes = c("x", "y", "strand", "label", "track", "individual",
    "species", "gene_id", "gene_name", "genomic_x", "genomic_xmin",
    "genomic_xmax", "strip_x_direction", "head_row")
)

.gene_lollipop_text <- function(d, i, ...) {
  label <- as.character(d$label[i])
  if (is.na(label)) label <- ""
  grid::textGrob(label, ..., gp = grid::gpar(fontsize = d$size[i] * ggplot2::.pt,
    fontfamily = d$family[i], fontface = d$fontface[i], lineheight = d$lineheight[i]))
}

#' @importFrom grid makeContent
#' @export
makeContent.geneLollipopGrob <- function(x) {
  d <- x$data
  p <- x$settings
  for (arg in c("head_height_mm", "tip_mm", "stem_height_mm", "anchor_tick_mm",
                "fan_height_mm", "lane_spacing_mm", "lane_clearance_mm", "stem_linewidth"))
    .genebox_positive_number(p[[arg]], arg)
  for (arg in c("padding_mm", "row_gap_mm", "stem_halo_mm")) {
    z <- p[[arg]]
    if (!is.numeric(z) || length(z) != 1L || !is.finite(z) || z < 0)
      stop("`", arg, "` must be a nonnegative finite number.", call. = FALSE)
  }
  measure <- function(z, dim) vapply(seq_len(nrow(z)), function(i) {
    g <- .gene_lollipop_text(z, i)
    if (dim == "width") grid::convertWidth(grid::grobWidth(g), "mm", valueOnly = TRUE)
    else grid::convertHeight(grid::grobHeight(g), "mm", valueOnly = TRUE)
  }, numeric(1))
  label_w <- measure(d, "width")
  needed_w <- max(measure(x$metrics, "width"), 0) + 2 * p$padding_mm + p$tip_mm
  w <- p$head_width_mm %||% needed_w
  .genebox_positive_number(w, "head_width_mm")
  if (w < needed_w - 1e-8)
    stop("Head width cannot fit labels; increase head_width_mm or use NULL.", call. = FALSE)
  h <- max(p$head_height_mm, max(measure(x$metrics, "height"), 0) + 2 * p$padding_mm)
  W <- grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  H <- grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE)
  a <- x$coords$x * W
  ay <- x$coords$y * H
  fixed_rows <- !all(is.na(d$head_row))
  outline <- if (fixed_rows) x$metrics$linewidth else d$linewidth
  stroke <- max(c(0, outline[is.finite(outline)]))
  row_gap <- p$row_gap_mm + stroke
  columns <- identical(p$head_layout, "columns")
  if (fixed_rows && !columns)
    stop("`head_row` requires head_layout = 'columns'.", call. = FALSE)
  routing_band <- if (columns) p$anchor_tick_mm + p$fan_height_mm else 0
  if (columns && (p$lane_spacing_mm < p$stem_linewidth ||
                  p$lane_clearance_mm < (p$stem_linewidth + stroke)/2))
    stop("Leader lane spacing and clearance must accommodate stem and head outlines.", call. = FALSE)
  room <- min(x$upper * H, H) - stroke / 2 - ay[1] - p$stem_height_mm - routing_band
  available_rows <- floor((room + row_gap + 1e-8) / (h + row_gap))
  if (available_rows < 1L)
    stop("Gene heads cannot fit vertically; increase output height or headroom.", call. = FALSE)
  # Reserve the centred outline on both panel edges and between heads.
  inset <- stroke / 2
  if (columns) {
    z <- .gene_lollipop_columns(a - inset, w, W - 2 * inset,
      p$gap_mm + stroke, p$max_shift_mm, min(p$max_rows, available_rows),
      p$lane_spacing_mm, p$lane_clearance_mm, head_row = d$head_row)
  } else {
    z <- .gene_lollipop_pack(a - inset, w, W - 2 * inset,
      p$gap_mm + stroke, p$max_shift_mm, min(p$max_rows, available_rows))
    z$column <- NA_integer_
    z$display_order <- rank(a, ties.method = "first")
    z$channel_x_mm <- z$head_x_mm
  }
  for (col in c("anchor_x_mm", "head_x_mm", "left_mm", "right_mm", "channel_x_mm"))
    z[[col]] <- z[[col]] + inset
  z$anchor_y_mm <- ay
  z$bottom_mm <- ay + routing_band + p$stem_height_mm + (z$row - 1L) * (h + row_gap)
  z$top_mm <- z$bottom_mm + h
  z$head_y_mm <- (z$bottom_mm + z$top_mm) / 2
  z$head_width_mm <- w
  z$head_height_mm <- h
  z$panel_width_mm <- W
  z$panel_height_mm <- H
  z$label_width_mm <- label_w
  z$direction <- x$direction
  z$genomic_x <- d$genomic_x
  z$label <- as.character(d$label)
  z$track <- d$track
  z$PANEL <- d$PANEL
  z$headroom_limit_mm <- min(x$upper * H, H)
  z$row_is_fixed <- fixed_rows
  z$head_layout <- p$head_layout
  z$tick_top_mm <- ay + p$anchor_tick_mm
  z$fan_top_mm <- z$tick_top_mm + p$fan_height_mm
  z$stem_colour <- as.character(d$stem_colour)
  auto <- is.na(z$stem_colour) | !nzchar(z$stem_colour)
  z$stem_colour[auto] <- .gene_lollipop_stem_colour(d$fill[auto])
  z$stem_linewidth_mm <- p$stem_linewidth
  z$tick_linewidth_mm <- min(.16, p$stem_linewidth)
  if (!is.null(p$layout_callback)) {
    if (!is.function(p$layout_callback)) stop("layout_callback must be a function.", call. = FALSE)
    p$layout_callback(z)
  }
  if (columns) {
    path_x <- as.vector(t(cbind(a, a, z$channel_x_mm, z$channel_x_mm, z$left_mm)))
    path_y <- as.vector(t(cbind(z$tick_top_mm, z$tick_top_mm, z$fan_top_mm,
                                z$head_y_mm, z$head_y_mm)))
    leader <- function(colour, width) grid::polylineGrob(
      x = grid::unit(path_x, "mm"), y = grid::unit(path_y, "mm"),
      id = rep(seq_len(nrow(d)), each = 5L),
      gp = grid::gpar(col = colour, lwd = width * ggplot2::.pt,
        lineend = "round", linejoin = "round"))
  } else {
    leader <- function(colour, width) grid::segmentsGrob(
      x0 = grid::unit(a, "mm"), y0 = grid::unit(ay, "mm"),
      x1 = grid::unit(z$head_x_mm, "mm"), y1 = grid::unit(z$bottom_mm, "mm"),
      gp = grid::gpar(col = colour, lwd = width * ggplot2::.pt, lineend = "round"))
  }
  children <- list(
    leader(scales::alpha(p$stem_halo_colour, d$alpha), p$stem_linewidth + 2 * p$stem_halo_mm),
    leader(scales::alpha(z$stem_colour, d$alpha), p$stem_linewidth))
  for (i in seq_len(nrow(d))) {
    l <- z$left_mm[i]; r <- z$right_mm[i]
    b <- z$bottom_mm[i]; t <- z$top_mm[i]; mid <- (b + t) / 2
    direction <- x$direction[i]
    if (direction == 0) {
      px <- c(l, r, r, l); py <- c(b, b, t, t)
    } else {
      px <- c(l, r - p$tip_mm, r, r - p$tip_mm, l)
      py <- c(b, b, mid, t, t)
      if (direction < 0) px <- l + r - px
    }
    children[[length(children) + 1L]] <- grid::polygonGrob(
      x = grid::unit(px, "mm"), y = grid::unit(py, "mm"),
      gp = grid::gpar(fill = scales::alpha(d$fill[i], d$alpha[i]),
        col = scales::alpha(d$colour[i], d$alpha[i]),
        lwd = d$linewidth[i] * ggplot2::.pt, lty = d$linetype[i], linejoin = "round"))
  }
  for (i in seq_len(nrow(d))) {
    g <- .gene_lollipop_text(d, i,
      x = grid::unit(z$head_x_mm[i] - x$direction[i] * p$tip_mm / 2, "mm"),
      y = grid::unit(z$head_y_mm[i], "mm"))
    col <- d$label_colour[i]
    if (is.na(col) || !nzchar(col)) col <- .genebox_contrast_colour(d$fill[i])
    g$gp$col <- scales::alpha(col, d$alpha[i])
    children[[length(children) + 1L]] <- g
  }
  children[[length(children) + 1L]] <- grid::segmentsGrob(
    x0 = grid::unit(a, "mm"), x1 = grid::unit(a, "mm"),
    y0 = grid::unit(ay, "mm"), y1 = grid::unit(z$tick_top_mm, "mm"),
    gp = grid::gpar(col = scales::alpha(z$stem_colour, d$alpha),
                   lwd = z$tick_linewidth_mm * ggplot2::.pt, lineend = "butt"))
  grid::setChildren(x, do.call(grid::gList, children))
}
