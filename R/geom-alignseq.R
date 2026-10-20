#' Annotate pairwise DNA differences with anchored sequence detail
#'
#' `geom_alignseq()` draws data prepared by [alignseq_data()] on annotation
#' panels. In marker mode, all nonmatching events in the selected regions are
#' drawn; contiguous indels have one marker, labelled with their length. Only
#' explicitly selected `details` windows receive DNA callouts on their respective annotation panels. Nearby
#' windows are merged by the preparation helper.
#'
#' Smooth curved connectors retain their exact genomic and callout endpoints.
#' Marker and connector anchors follow [scale_x_ggexon_genomic()] automatically.
#' Callout columns are equally spaced in panel space **after** genomic scaling:
#' they are schematic alignment detail, not an additional genomic ruler.
#' Each panel displays only its own sequence in the supplied orientation.
#' Markers describe the query relative to the reference, not evolutionary change.
#'
#' @param data A table returned by [alignseq_data()]. Must be supplied explicitly.
#'   Optional `boundary_after` (logical) and `boundary_label` (character) columns
#'   mark verified boundaries, such as splice donors, after selected bases in
#'   the equally spaced sequence detail. These do not alter the DNA strings.
#' @param mapping Optional aesthetic overrides. Prepared metadata and x/y/track
#'   are mapped automatically. Use anchor `track` names matching the annotation
#'   facet, never a link-panel track.
#' @param mode `"markers"` (default) or `"rows"`. Rows display all selected
#'   columns as an equally spaced DNA row in each sequence's own annotation panel,
#'   connected to its genomic extent.
#'   Row mode requires one contiguous alignment-column selection per sequence.
#' @param size DNA character size in mm.
#' @param family Character font, default `"mono"`.
#' @param background Logical; draw pale nucleotide backgrounds.
#' @param max_columns Maximum columns in a callout or row view (default 60).
#'   Longer windows raise an error requesting a narrower selection.
#' @param na.rm,show.legend,... Standard layer arguments. Event colours and
#'   shapes are fixed: substitution blue circle, insertion orange triangle,
#'   deletion purple inverted triangle, ambiguous grey cross. No fill scale is
#'   added, so existing exon and link palettes remain available.
#' @return A ggplot2 layer using a native annotation geom. Cartesian coordinates
#'   are required. In ggexon, use with annotation data and [facet_genomics()].
#' @seealso [alignseq_data()], [geom_exon2()], [scale_x_ggexon_genomic()]
#' @export
geom_alignseq <- function(data, mapping = NULL, mode = c("markers", "rows"),
                          size = 2.5, family = "mono", background = TRUE,
                          max_columns = 60, na.rm = FALSE, show.legend = FALSE, ...) {
  mode <- match.arg(mode)
  fields <- c("x", "y", "track", "sequence", "reference", "query", "column", "base",
    "reference_base", "query_base", "event_type", "event_id", "event_length", "detail_id")
  if (missing(data) || !is.data.frame(data) || !all(fields %in% names(data)))
    stop("Supply a table prepared by alignseq_data().", call. = FALSE)
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0)
    stop("size must be positive.", call. = FALSE)
  if (!is.numeric(max_columns) || length(max_columns) != 1L || !is.finite(max_columns) || max_columns < 1)
    stop("max_columns must be positive.", call. = FALSE)
  groups <- if (mode == "rows") split(data$column, data$sequence) else
    split(data$column, interaction(data$sequence, data$detail_id, drop = TRUE))
  if (mode == "rows" && any(vapply(groups, function(x)
      any(diff(sort(unique(x))) != 1L), logical(1))))
    stop("Row mode requires a contiguous window; plot disjoint regions separately.", call. = FALSE)
  if (any(vapply(groups, function(x) length(unique(x)) > max_columns, logical(1))))
    stop("Sequence detail exceeds max_columns; select narrower regions or details.", call. = FALSE)
  if (!"boundary_after" %in% names(data)) data$boundary_after <- rep(FALSE, nrow(data))
  if (!"boundary_label" %in% names(data)) data$boundary_label <- rep("", nrow(data))
  fields <- c(fields, "boundary_after", "boundary_label")
  defaults <- ggplot2::aes(!!!stats::setNames(lapply(fields, rlang::sym), fields))
  for (nm in names(mapping)) defaults[[nm]] <- mapping[[nm]]
  ggplot2::layer(data = data, mapping = defaults, stat = "identity", position = "identity",
    geom = GeomAlignseq, inherit.aes = FALSE, show.legend = show.legend,
    params = list(mode = mode, size = size, family = family, background = background,
      na.rm = na.rm, ...))
}

GeomAlignseq <- ggplot2::ggproto("GeomAlignseq", ggplot2::Geom,
  ggexon_panel_role = "annotation",
  required_aes = c("x", "y"),
  optional_aes = c("track", "sequence", "reference", "query", "column", "base",
    "reference_base", "query_base", "event_type", "event_id", "event_length", "detail_id",
    "boundary_after", "boundary_label"),
  default_aes = ggplot2::aes(colour = "grey20", alpha = NA),
  extra_params = c("na.rm", "mode", "size", "family", "background"),
  setup_data = function(data, params) {
    # Reserve vertical space for callouts without adding synthetic x positions.
    count <- length(unique(stats::na.omit(data$detail_id)))
    data$ymin <- data$y - .15
    data$ymax <- data$y + (if (params$mode == "rows") .95 else if (count) .95 + .65 * (count - 1) else .5) +
      if (any(data$boundary_after %in% TRUE)) .35 else 0
    data
  },
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_params, coord, mode = "markers", size = 2.5,
                        family = "mono", background = TRUE, na.rm = FALSE) {
    if (!nrow(data)) return(grid::zeroGrob())
    if (!inherits(coord, "CoordCartesian") || inherits(coord, "CoordFlip"))
      stop("geom_alignseq() requires unflipped Cartesian coordinates.", call. = FALSE)
    grobs <- list()
    # Multiple sequence tracks can share an annotation panel; each y row is independent.
    tracks <- split(data, interaction(match(data$sequence, unique(data$sequence)),
      match(data$y, unique(data$y)), drop = TRUE))
    for (d in tracks) {
      d <- d[order(d$column), , drop = FALSE]
      base_y <- d$y[1]
      if (mode == "markers") {
        ev <- d[!is.na(d$event_id), , drop = FALSE]
        if (nrow(ev)) {
          ev <- do.call(rbind, lapply(split(ev, ev$event_id), function(e) {
            e$x[1] <- mean(range(e$x)); e[1, , drop = FALSE]
          }))
          colours <- c(substitution = "#0072B2", insertion = "#D55E00", deletion = "#CC79A7", ambiguous = "grey55")
          shapes <- c(substitution = 16, insertion = 17, deletion = 25, ambiguous = 4)
          ev$y <- base_y + .2
          ev$colour <- unname(colours[ev$event_type]); ev$fill <- ev$colour
          ev$shape <- unname(shapes[ev$event_type]); ev$size <- 1.8; ev$stroke <- .35
          grobs[[length(grobs) + 1L]] <- ggplot2::GeomPoint$draw_panel(ev, panel_params, coord)
          indel <- ev[ev$event_type %in% c("insertion", "deletion"), , drop = FALSE]
          if (nrow(indel)) {
            indel$label <- paste0(ifelse(indel$event_type == "insertion", "+", "\u2212"), indel$event_length)
            indel$y <- base_y + .48
            grobs[[length(grobs) + 1L]] <- .alignseq_text(indel, panel_params, coord, size, family)
          }
        }
      }
      chunks <- if (mode == "rows") list(d) else split(d, d$detail_id, drop = TRUE)
      if (!length(chunks)) next
      # Layout in panel fractions; connectors still start at transformed genomic x.
      range_x <- panel_params$x.range %||% panel_params$x$continuous_range
      from_fraction <- function(x) {
        if ((panel_params$reverse %||% "none") %in% c("x", "xy")) x <- 1 - x
        range_x[1] + x * diff(range_x)
      }
      occupied <- list()
      for (chunk in chunks) {
        chunk <- chunk[!duplicated(chunk$column), , drop = FALSE]
        n <- nrow(chunk)
        width <- min(.70, max(.16, n * .027))
        anchor_fraction <- coord$transform(data.frame(x = mean(range(chunk$x)), y = base_y), panel_params)$x
        ideal <- min(1 - width / 2 - .03, max(.22 + width / 2, anchor_fraction))
        candidates <- unique(c(ideal, seq(.22 + width / 2, .97 - width / 2, length.out = 100)))
        candidates <- candidates[order(abs(candidates - ideal))]
        lane <- 1L
        repeat {
          free <- vapply(candidates, function(centre) {
            left <- centre - width / 2; right <- centre + width / 2
            lane > length(occupied) || !any(vapply(occupied[[lane]],
              function(z) left - .19 < z[2] + .03 && right > z[1] - .03, logical(1)))
          }, logical(1))
          if (any(free)) break
          lane <- lane + 1L
        }
        centre <- candidates[which(free)[1]]
        left <- centre - width / 2; right <- centre + width / 2
        if (lane > length(occupied)) occupied[[lane]] <- list()
        occupied[[lane]][[length(occupied[[lane]]) + 1L]] <- c(left - .19, right)
        y0 <- base_y + .7 + (lane - 1) * .65
        xs <- from_fraction(left + (seq_len(n) - .5) * width / n)
        ends <- chunk$x[c(1L, n)]
        connectors <- data.frame(x = ends, y = base_y + .23,
          xend = from_fraction(c(left, right)), yend = y0 - .13,
          colour = "grey60", linewidth = .25, linetype = 1, alpha = NA)
        curves <- .generate_lollipop_stems(connectors$x, connectors$y,
          connectors$xend, connectors$yend, n = 60, k = .9)
        paths <- data.frame(x = curves$lollipop_x, y = curves$lollipop_y,
          group = curves$lollipop_curve_id, colour = "grey60", linewidth = .25,
          linetype = 1, alpha = NA)
        grobs[[length(grobs) + 1L]] <- ggplot2::GeomPath$draw_panel(paths, panel_params, coord)
        for (row in 1L) {
          letters <- chunk$base
          chars <- data.frame(x = xs, y = y0,
            label = letters, colour = ifelse(chunk$event_type == "match", "grey45", "grey10"))
          if (isTRUE(background)) {
            nt <- c(A = "#D9EDD7", C = "#D9EAF5", G = "#FAEBC5", T = "#F4DADB", `-` = "#EEEEEE")
            fill <- unname(nt[letters]); fill[is.na(fill)] <- "#EEEEEE"
            boxes <- data.frame(xmin = xs - diff(range_x) * width / n / 2,
              xmax = xs + diff(range_x) * width / n / 2,
              ymin = chars$y - .15, ymax = chars$y + .15,
              fill = fill, colour = NA, linewidth = 0, linetype = 1, alpha = NA)
            grobs[[length(grobs) + 1L]] <- ggplot2::GeomRect$draw_panel(boxes, panel_params, coord)
          }
          grobs[[length(grobs) + 1L]] <- .alignseq_text(chars, panel_params, coord, size, family)
          for (j in which(chunk$boundary_after %in% TRUE)) {
            boundary_x <- from_fraction(left + j * width / n)
            edge <- data.frame(x = boundary_x, xend = boundary_x,
              y = y0 - .21, yend = y0 + .21,
              colour = "grey15", linewidth = .4, linetype = 1, alpha = NA)
            grobs[[length(grobs) + 1L]] <- ggplot2::GeomSegment$draw_panel(edge, panel_params, coord)
            if (!is.na(chunk$boundary_label[j]) && nzchar(chunk$boundary_label[j])) {
              tag <- data.frame(x = boundary_x, y = y0 + .34,
                label = chunk$boundary_label[j], colour = "grey15")
              grobs[[length(grobs) + 1L]] <- .alignseq_text(tag, panel_params, coord, size, family)
            }
          }
          name <- if (chunk$sequence[1] == chunk$reference[1])
            paste0(chunk$sequence[1], " (reference)") else chunk$sequence[1]
          label <- data.frame(x = from_fraction(left - .015), y = chars$y[1], label = name, colour = "grey20")
          grobs[[length(grobs) + 1L]] <- .alignseq_text(label, panel_params, coord, size * .85, family, hjust = 1)
        }
      }
    }
    grid::gTree(children = do.call(grid::gList, grobs), name = "geom_alignseq")
  }
)

.alignseq_text <- function(data, panel_params, coord, size, family, hjust = .5) {
  data$size <- size; data$family <- family; data$fontface <- 1
  data$angle <- 0; data$hjust <- hjust; data$vjust <- .5
  data$lineheight <- 1; data$alpha <- NA
  ggplot2::GeomText$draw_panel(data, panel_params, coord)
}
