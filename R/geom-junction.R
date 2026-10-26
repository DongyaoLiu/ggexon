#' Draw splice junction arcs on annotation or coverage panels
#'
#' `geom_junction()` connects genomic donor and acceptor anchors with an upward
#' arc. Map read counts to `label` and `linewidth` to show splice evidence.
#' Counts describe junction support, not full transcript abundance.
#'
#' With [scale_x_ggexon_genomic()], endpoints consume the exon-trained map and
#' arcs are generated afterwards. Labels sit halfway between the displayed
#' endpoints, on the curve. The layer does not train exon boundaries. In Syn-backed
#' [facet_genomics()] plots, `panel` selects the annotation or coverage panel
#' matching each junction's `track`. It also works in ordinary ggplot2 plots,
#' where `panel` does not create a separate panel. Map `yend` to anchor the end
#' independently; it defaults to `y`. The baseline interpolates linearly between
#' the endpoint heights in displayed x coordinates, and the curve rises above
#' it for either endpoint order.
#'
#' @param mapping,data,...,na.rm,show.legend,inherit.aes Standard ggplot2 layer
#'   arguments. Required aesthetics are `x`, `xend`, and `y`. Optional aesthetics
#'   include `yend`, `track`, `height`, `label`, `hjust`, `linewidth`, `colour`,
#'   `linetype`, and `alpha`. Use splice-boundary anchors in the annotation
#'   coordinate system. Labels default to centered horizontal justification
#'   (`hjust = 0.5`); map or set `hjust` to reposition crowded labels.
#' @param height Positive, finite rise above the line between the endpoints at
#'   their displayed x midpoint, in the selected panel's y units, default 0.6.
#'   The curve's maximum may lie away from the midpoint when `yend` differs
#'   from `y`. Alternatively
#'   map `height` to give junctions different rises. An explicitly supplied
#'   constant overrides the mapping. The layer reserves vertical room for the
#'   arc and an additional quarter of its rise when a label is present.
#' @param label_size Label text size in mm, default 2.5.
#' @param label_family Label font family.
#' @param label_vjust Vertical label justification relative to the curve at
#'   the displayed x midpoint,
#'   default -0.3. Labels inherit the arc colour and alpha.
#' @param panel Target panel, either `"annotation"` (the default) or
#'   `"coverage"`. Use `"coverage"` to align arcs with a BigWig track. Map
#'   `track` to the corresponding coverage track and express `y`, `yend`, and `height`
#'   in its signal units. Junctions reserve vertical room under ordinary scale
#'   training, but do not affect signal-derived automatic coverage limits.
#'   Explicit or automatic [scale_panel_coverage()] limits remain authoritative;
#'   choose suitable arc positions or explicit limits to retain headroom.
#' @return A ggplot2 layer.
#' @seealso [geom_coverage()], [geom_exon2()], [scale_x_ggexon_genomic()]
#' @examples
#' junctions <- data.frame(donor = c(100, 100), acceptor = c(300, 700),
#'   y = c(1, 2), reads = c(45, 21))
#' ggplot2::ggplot(junctions, ggplot2::aes(x = donor, xend = acceptor, y = y)) +
#'   geom_junction(ggplot2::aes(label = reads, linewidth = reads))
#' @export
geom_junction <- function(mapping = NULL, data = NULL, ..., height = .6,
                          label_size = 2.5, label_family = "", label_vjust = -.3,
                          panel = c("annotation", "coverage"),
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  panel <- match.arg(panel)
  if (!is.numeric(height) || length(height) != 1L ||
      !is.finite(height) || height <= 0)
    stop("height must be a positive, finite number.", call. = FALSE)
  if (!is.numeric(label_size) || length(label_size) != 1L ||
      !is.finite(label_size) || label_size <= 0)
    stop("label_size must be a positive, finite number.", call. = FALSE)
  if (!is.character(label_family) || length(label_family) != 1L || is.na(label_family))
    stop("label_family must be a single font-family string.", call. = FALSE)
  if (!is.numeric(label_vjust) || length(label_vjust) != 1L || !is.finite(label_vjust))
    stop("label_vjust must be a finite number.", call. = FALSE)
  params <- list(..., label_size = label_size, label_family = label_family,
    label_vjust = label_vjust, na.rm = na.rm)
  if (!missing(height)) params$height <- height
  geom <- ggplot2::ggproto(NULL, GeomJunction, ggexon_panel_role = panel)
  ggplot2::layer(mapping = mapping, data = data, geom = geom,
    stat = "identity", position = "identity", inherit.aes = inherit.aes,
    show.legend = show.legend, params = params)
}

GeomJunction <- ggplot2::ggproto("GeomJunction", ggplot2::Geom,
  ggexon_panel_role = "annotation",
  ggexon_coverage_signal = FALSE,
  required_aes = c("x", "xend", "y"),
  optional_aes = c("track", "yend"),
  non_missing_aes = "yend",
  default_aes = ggplot2::aes(colour = "grey30", linewidth = .5,
    linetype = 1, alpha = NA, height = .6, label = NA_character_, hjust = .5),
  extra_params = c("na.rm", "label_size", "label_family", "label_vjust"),
  setup_data = function(data, params) {
    height <- params$height %||% data$height %||% .6
    if (!is.numeric(height) || any(!is.finite(height) | height <= 0))
      stop("height must contain only positive, finite numbers.", call. = FALSE)
    data$height <- height
    data$y <- params$y %||% data$y
    data$yend <- params$yend %||% data$yend %||% data$y
    if (!is.numeric(data$yend) || any(is.infinite(data$yend)))
      stop("yend must contain only finite numbers or missing values.", call. = FALSE)
    label <- params$label %||% data$label
    labelled <- if (is.null(label)) FALSE else !is.na(label) & nzchar(as.character(label))
    data$ymin <- pmin(data$y, data$yend)
    # The baseline follows x(t) = 3t^2 - 2t^3; rise is 4 * height * t * (1-t).
    difference <- data$yend - data$y
    magnitude <- pmax(abs(difference), data$height)
    delta <- difference / magnitude
    rise <- data$height / magnitude
    peak <- .5 + 1.5 * delta / (sqrt(9 * delta^2 + 16 * rise^2) + 4 * rise)
    baseline <- peak^2 * (3 - 2 * peak)
    data$ymax <- (1 - baseline) * data$y + baseline * data$yend +
      data$height * (4 * peak * (1 - peak)) +
      ifelse(labelled, data$height / 4, 0)
    data
  },
  draw_key = ggplot2::draw_key_path,
  draw_panel = function(data, panel_params, coord, label_size = 2.5,
                        label_family = "", label_vjust = -.3, na.rm = FALSE) {
    if (!nrow(data)) return(grid::zeroGrob())
    paths <- lapply(seq_len(nrow(data)), function(i) {
      d <- data[i, , drop = FALSE]
      control_rise <- d$height * (4 / 3)
      curve <- .bezier_curve(d$x, d$y, d$x, d$y + control_rise,
        d$xend, d$yend + control_rise, d$xend, d$yend, n = 61L)
      path <- d[rep(1L, nrow(curve)), , drop = FALSE]
      path$x <- curve$x; path$y <- curve$y; path$group <- i
      path
    })
    grobs <- list(ggplot2::GeomPath$draw_panel(do.call(rbind, paths), panel_params, coord))
    labels <- data[!is.na(data$label) & nzchar(as.character(data$label)), , drop = FALSE]
    if (nrow(labels)) {
      labels$x <- (labels$x + labels$xend) / 2
      labels$y <- (labels$y + labels$yend) / 2 + labels$height
      labels$size <- label_size; labels$family <- label_family
      labels$fontface <- 1; labels$angle <- 0; labels$lineheight <- 1
      labels$vjust <- label_vjust
      grobs[[2L]] <- ggplot2::GeomText$draw_panel(labels, panel_params, coord)
    }
    grid::gTree(children = do.call(grid::gList, grobs), name = "geom_junction")
  }
)
