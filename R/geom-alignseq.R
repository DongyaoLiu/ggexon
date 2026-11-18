#' Annotate pairwise DNA differences with anchored sequence detail
#'
#' `geom_alignseq()` draws DNA prepared by [prepare_alignseq()] or [alignseq_data()] on annotation
#' panels. In marker mode, all nonmatching events in the selected regions are
#' drawn; contiguous indels have one marker, labelled with their length. Only
#' explicitly selected `details` windows receive DNA callouts on their respective annotation panels. Nearby
#' windows are merged by the preparation helper.
#'
#' Each sequence block has one smooth curved connector from the midpoint of
#' its displayed genomic interval to the bottom centre of the sequence block.
#' Marker and connector anchors follow [scale_x_ggexon_genomic()] automatically.
#' Callout columns are equally spaced in panel space **after** genomic scaling:
#' they are schematic alignment detail, not an additional genomic ruler.
#' Each panel displays only its own sequence in the supplied orientation.
#' Markers describe the query relative to the reference, not evolutionary change.
#'
#' @param data A table returned by [alignseq_data()], a Syn object, or NULL to
#'   use the Syn object supplied to [ggexon()]. For explicit tables,
#'   `track` identifies the receiving annotation panel.
#'   Optional `boundary_after` (logical) and `boundary_label` (character) columns
#'   mark verified boundaries, such as splice donors, after selected bases in
#'   the equally spaced sequence detail. These do not alter the DNA strings.
#' @param alignment Name stored by [prepare_alignseq()]. Omit when the selected
#'   individuals have one applicable preparation.
#' @param species,chr,subset Optional selectors for Syn input. Select individuals
#'   or annotation tracks with `species`, chromosomes with `chr` and an inclusive
#'   length-two interval in native coordinates with `subset`.
#' @param show_markers Draw event points and their length labels in marker mode.
#' @param anchor Connector origin. `"track"` uses the prepared sequence row.
#'   `"transcript"` uses the displayed exon edge, or the transcript backbone
#'   between exons. This requires a matching exon layer in [ggexon()].
#' @param transcripts Named character vector mapping individual IDs to source
#'   transcript IDs for `anchor = "transcript"`. Source IDs remain usable when
#'   transcript display labels change. For explicit tables without an individual
#'   column, use track names and the displayed `transcripts` values.
#' @param mapping Optional aesthetic overrides. Prepared metadata and x/y/track
#'   are mapped automatically. Use anchor `track` names matching the annotation
#'   facet, never a link-panel track.
#' @param mode `"markers"` (default) or `"rows"`. Rows display all selected
#'   columns as an equally spaced DNA row in each sequence's own annotation panel,
#'   connected to the midpoint of its displayed genomic interval.
#'   Row mode requires one contiguous alignment-column selection per sequence.
#' @param sequence_position Horizontal placement of sequence blocks:
#'   `"auto"` (default), `"left"`, `"center"`, or `"right"`. Automatic placement
#'   stays near the genomic anchor and moves blocks apart before stacking.
#'   Explicit positions keep their horizontal alignment and stack overlapping
#'   blocks vertically. Placement is in panel space after genomic scaling,
#'   including on reversed panels. Blocks are kept within the available space
#'   with room for sequence names, so wide centred blocks may shift right to fit.
#' @param size DNA character size in mm.
#' @param family Character font, default `"mono"`.
#' @param background Logical; draw nucleotide backgrounds.
#' @param colour_scheme `"nucleotide"` (default) colours A/C/G/T bases
#'   separately. `"difference"` uses a neutral background for aligned columns,
#'   one colour for substitutions, and another for insertions and deletions.
#'   Both sequences highlight the same differing alignment columns. Ambiguous
#'   IUPAC bases remain neutral rather than being called verified mismatches.
#' @param difference_colours Named character vector of three colours with keys
#'   `aligned`, `mismatch`, and `indel`. Used with `colour_scheme = "difference"`.
#'   Defaults to grey aligned columns, orange mismatches, and red indels.
#'   Text switches between black and white for readable contrast, assuming a
#'   white panel background when colours are translucent. With
#'   `background = FALSE`, mismatch and indel colours apply to the letters,
#'   while aligned letters retain their neutral grey shade.
#' @param max_columns Maximum columns in a callout or row view (default 60).
#'   Longer windows raise an error requesting a narrower selection.
#' @param na.rm,show.legend,... Standard layer arguments. Event colours and
#'   shapes use the default nucleotide scheme: substitution blue circle,
#'   insertion orange triangle, deletion purple inverted triangle, ambiguous
#'   grey cross. The difference scheme also uses its palette for substitution
#'   and indel markers. No fill scale is
#'   added, so existing exon and link palettes remain available.
#' @return A ggplot2 layer using a native annotation geom. Cartesian coordinates
#'   are required. In ggexon, use with annotation data and [facet_genomics()].
#' @seealso [alignseq_data()], [geom_exon2()], [scale_x_ggexon_genomic()]
#' @export
geom_alignseq <- function(data = NULL, mapping = NULL, mode = c("markers", "rows"),
                          size = 2.5, family = "mono", background = TRUE,
                          max_columns = 60, na.rm = FALSE, show.legend = FALSE, ...,
                          sequence_position = c("auto", "left", "center", "right"),
                          colour_scheme = c("nucleotide", "difference"),
                          difference_colours = c(aligned = "#E6E6E6",
                            mismatch = "#E69F00", indel = "#D73027"),
                          alignment = NULL, species = NULL, chr = NULL, subset = NULL,
                          show_markers = TRUE, anchor = c("track", "transcript"),
                          transcripts = NULL) {
  mode <- match.arg(mode)
  sequence_position <- match.arg(sequence_position)
  colour_scheme <- match.arg(colour_scheme)
  if (colour_scheme == "difference") .alignseq_validate_difference_colours(difference_colours)
  anchor <- match.arg(anchor)
  if (!is.logical(show_markers) || length(show_markers) != 1L || is.na(show_markers))
    stop("Supply one logical value for show_markers.", call. = FALSE)
  if (anchor == "transcript" && (!is.character(transcripts) || !length(transcripts) ||
      is.null(names(transcripts)) || anyNA(names(transcripts)) || any(!nzchar(names(transcripts))) ||
      anyDuplicated(names(transcripts)) || anyNA(transcripts) || any(!nzchar(transcripts))))
    stop("Supply transcripts as a named character vector mapping individuals to transcript IDs.", call. = FALSE)
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0)
    stop("size must be positive.", call. = FALSE)
  if (!is.numeric(max_columns) || length(max_columns) != 1L || !is.finite(max_columns) || max_columns < 1)
    stop("max_columns must be positive.", call. = FALSE)
  if (!is.null(subset) && (!is.numeric(subset) || length(subset) != 2L || any(!is.finite(subset))))
    stop("Supply a finite length-two genomic subset.", call. = FALSE)
  if (!is.null(chr) && (!is.character(chr) || anyNA(chr) || any(!nzchar(chr))))
    stop("Supply chromosome names for chr.", call. = FALSE)
  fields <- GeomAlignseq$syn_default_aes
  defaults <- mapping
  if (is.data.frame(data)) {
    required <- setdiff(fields, c("boundary_after", "boundary_label", "individual"))
    if (!all(required %in% names(data))) stop("Supply a table prepared by alignseq_data().", call. = FALSE)
    .alignseq_validate_chunks(data, mode, max_columns)
    if (!"boundary_after" %in% names(data)) data$boundary_after <- rep(FALSE, nrow(data))
    if (!"boundary_label" %in% names(data)) data$boundary_label <- rep("", nrow(data))
    defaults <- syn_identity_mapping(intersect(fields, names(data)))
    for (nm in names(mapping)) defaults[[nm]] <- mapping[[nm]]
  }
  ggplot2::layer(data = data, mapping = defaults, stat = "identity", position = "identity",
    geom = GeomAlignseq, layer_class = LayerSyn, inherit.aes = FALSE, show.legend = show.legend,
    params = list(max_columns = max_columns, alignment = alignment, species = species, chr = chr, subset = subset,
      show_markers = show_markers, anchor = anchor, transcripts = transcripts, mode = mode, sequence_position = sequence_position,
      colour_scheme = colour_scheme, difference_colours = difference_colours,
      size = size, family = family, background = background,
      na.rm = na.rm, ...))
}

.alignseq_validate_chunks <- function(data, mode, max_columns) {
  groups <- if (mode == "rows") split(data$column, data$sequence) else
    split(data$column, interaction(data$sequence, data$detail_id, drop = TRUE))
  if (mode == "rows" && any(vapply(groups, function(x)
      any(diff(sort(unique(x))) != 1L), logical(1))))
    stop("Row mode requires a contiguous window; plot disjoint regions separately.", call. = FALSE)
  if (any(vapply(groups, function(x) length(unique(x)) > max_columns, logical(1))))
    stop("Sequence detail exceeds max_columns; select narrower regions or details.", call. = FALSE)
}

GeomAlignseq <- ggplot2::ggproto("GeomAlignseq", ggplot2::Geom,
  ggexon_panel_role = "annotation",
  syn_data = function(x, layer) syn_alignseq_data(x, layer),
  syn_default_aes = c("x", "y", "track", "individual", "sequence", "reference", "query",
    "column", "base", "reference_base", "query_base", "event_type", "event_id",
    "event_length", "detail_id", "boundary_after", "boundary_label"),
  required_aes = c("x", "y"),
  chromosome_columns = c("chr", "seqnames"),
  optional_aes = c("source_chr", "track", "individual", "sequence", "reference", "query", "column", "base",
    "reference_base", "query_base", "event_type", "event_id", "event_length", "detail_id",
    "boundary_after", "boundary_label"),
  default_aes = ggplot2::aes(colour = "grey20", alpha = NA),
  extra_params = c("max_columns", "na.rm", "mode", "sequence_position", "colour_scheme",
    "difference_colours", "size", "family", "background", "alignment", "species", "chr",
    "subset", "anchor", "transcripts", "show_markers"),
  setup_data = function(data, params) {
    data <- data[!is.na(data$column), , drop = FALSE]
    # Reserve vertical space for callouts without adding synthetic x positions.
    count <- length(unique(stats::na.omit(data$detail_id)))
    data$ymin <- data$y - .15
    data$ymax <- data$y + (if (params$mode == "rows") .95 else if (count) .95 + .65 * (count - 1) else .5) +
      if (any(data$boundary_after %in% TRUE)) .35 else 0
    data
  },
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_params, coord, mode = "markers", size = 2.5,
                        family = "mono", background = TRUE, na.rm = FALSE, show_markers = TRUE,
                        anchor = "track",
                        sequence_position = "auto", colour_scheme = "nucleotide",
                        difference_colours = c(aligned = "#E6E6E6",
                          mismatch = "#E69F00", indel = "#D73027")) {
    if (!nrow(data)) return(grid::zeroGrob())
    if (!inherits(coord, "CoordCartesian") || inherits(coord, "CoordFlip"))
      stop("geom_alignseq() requires unflipped Cartesian coordinates.", call. = FALSE)
    if (anchor == "transcript" && !"yend" %in% names(data))
      stop("Use ggexon() with a displayed exon layer for transcript anchoring.", call. = FALSE)
    grobs <- list()
    stem_grobs <- list()
    # Multiple sequence tracks can share an annotation panel; each y row is independent.
    tracks <- split(data, interaction(match(data$sequence, unique(data$sequence)),
      match(data$y, unique(data$y)), drop = TRUE))
    for (d in tracks) {
      d <- d[order(d$column), , drop = FALSE]
      base_y <- d$y[1]
      if (mode == "markers" && show_markers) {
        ev <- d[!is.na(d$event_id), , drop = FALSE]
        if (nrow(ev)) {
          ev <- do.call(rbind, lapply(split(ev, ev$event_id), function(e) {
            e$x[1] <- mean(range(e$x)); e[1, , drop = FALSE]
          }))
          colours <- c(substitution = "#0072B2", insertion = "#D55E00", deletion = "#CC79A7", ambiguous = "grey55")
          if (colour_scheme == "difference") {
            colours[c("substitution", "insertion", "deletion")] <-
              difference_colours[c("mismatch", "indel", "indel")]
          }
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
        anchor_x <- if (anchor == "transcript") chunk$xend[1] else mean(range(chunk$x))
        anchor_fraction <- coord$transform(data.frame(x = anchor_x, y = base_y), panel_params)$x
        candidates <- .alignseq_position_candidates(width, anchor_fraction, sequence_position)
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
        connectors <- data.frame(x = anchor_x, y = if (anchor == "transcript") chunk$yend[1] else base_y + .23,
          xend = from_fraction(centre), yend = y0 - .15,
          colour = "grey60", linewidth = .25, linetype = 1, alpha = NA)
        curves <- .generate_lollipop_stems(connectors$x, connectors$y,
          connectors$xend, connectors$yend, n = 60, k = .9)
        paths <- data.frame(x = curves$lollipop_x, y = curves$lollipop_y,
          group = curves$lollipop_curve_id, colour = "grey60", linewidth = .25,
          linetype = 1, alpha = NA)
        stem_grobs[[length(stem_grobs) + 1L]] <- ggplot2::GeomPath$draw_panel(paths, panel_params, coord)
        for (row in 1L) {
          letters <- chunk$base
          palette <- .alignseq_base_colours(chunk, colour_scheme, difference_colours, background)
          chars <- data.frame(x = xs, y = y0,
            label = letters, colour = palette$text)
          if (isTRUE(background)) {
            boxes <- data.frame(xmin = xs - diff(range_x) * width / n / 2,
              xmax = xs + diff(range_x) * width / n / 2,
              ymin = chars$y - .15, ymax = chars$y + .15,
              fill = palette$fill, colour = NA, linewidth = 0, linetype = 1, alpha = NA)
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
    # Stems from upper lanes must not paint over lower sequence blocks.
    grid::gTree(children = do.call(grid::gList, c(stem_grobs, grobs)), name = "geom_alignseq")
  }
)

.alignseq_validate_difference_colours <- function(colours) {
  keys <- c("aligned", "mismatch", "indel")
  if (!is.character(colours) || length(colours) != 3L || anyNA(colours) ||
      any(!nzchar(colours)) || anyDuplicated(names(colours)) ||
      !setequal(names(colours), keys)) {
    stop("`difference_colours` must supply named colours for aligned, mismatch, and indel.", call. = FALSE)
  }
  tryCatch(grDevices::col2rgb(colours), error = function(e) {
    stop("`difference_colours` contains an invalid colour.", call. = FALSE)
  })
  invisible(NULL)
}

.alignseq_base_colours <- function(chunk, scheme, colours, background) {
  text <- ifelse(chunk$event_type == "match", "grey45", "grey10")
  if (scheme == "nucleotide") {
    nt <- c(A = "#D9EDD7", C = "#D9EAF5", G = "#FAEBC5", T = "#F4DADB", `-` = "#EEEEEE")
    fill <- unname(nt[chunk$base])
    fill[is.na(fill)] <- "#EEEEEE"
  } else {
    event <- rep("aligned", nrow(chunk))
    event[chunk$event_type %in% "substitution"] <- "mismatch"
    event[chunk$event_type %in% c("insertion", "deletion")] <- "indel"
    fill <- unname(colours[event])
    if (isTRUE(background)) {
      rgba <- grDevices::col2rgb(fill, alpha = TRUE) / 255
      # Composite translucent fills over the default white panel before
      # choosing glyph contrast.
      rgb <- sweep(rgba[1:3, , drop = FALSE], 2, rgba[4, ], "*")
      rgb <- sweep(rgb, 2, 1 - rgba[4, ], "+")
      rgb <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
      luminance <- colSums(rgb * c(0.2126, 0.7152, 0.0722))
      text <- ifelse(luminance < sqrt(0.0525) - 0.05, "white", "black")
    } else {
      changed <- event != "aligned"
      text[changed] <- unname(colours[event[changed]])
    }
  }
  list(fill = fill, text = unname(text))
}

.alignseq_position_candidates <- function(width, anchor, sequence_position) {
  lower <- .22 + width / 2
  upper <- .97 - width / 2
  ideal <- switch(sequence_position,
    auto = anchor, left = lower, center = .5, right = upper)
  ideal <- min(upper, max(lower, ideal))
  if (sequence_position != "auto") return(ideal)
  candidates <- unique(c(ideal, seq(lower, upper, length.out = 100)))
  candidates[order(abs(candidates - ideal))]
}

.alignseq_text <- function(data, panel_params, coord, size, family, hjust = .5) {
  data$size <- size; data$family <- family; data$fontface <- 1
  data$angle <- 0; data$hjust <- hjust; data$vjust <- .5
  data$lineheight <- 1; data$alpha <- NA
  ggplot2::GeomText$draw_panel(data, panel_params, coord)
}
