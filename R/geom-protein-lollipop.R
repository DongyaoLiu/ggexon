#' Prepare lollipop annotation data on a linear feature track
#'
#' `protein_lollipop_data()` prepares the small data frames needed to draw a
#' mutation lollipop plot. Coordinates are treated as positions on one linear
#' track, so the same structure can be used for amino-acid positions, CDS
#' positions, or nucleotide positions after upstream coordinate conversion.
#'
#' @param mutations A data frame with one row per mutation.
#' @param domains Optional interval table, `S4Vectors::DataFrame`, or
#'   `SynProteinDomainAnnotation` object. Intervals must use the same coordinate
#'   system as `mutations`.
#' @param protein_length Optional track length. When `NULL`, the maximum
#'   mutation position/domain end is used.
#' @param position Column in `mutations` containing mutation positions.
#' @param label Optional label column in `mutations`. When omitted, a
#'   `mutation` column is used if present; otherwise labels are built from
#'   `ref`, `position`, and `alt` when both amino-acid columns are supplied.
#' @param ref,alt Optional reference and alternate residue/base columns used to
#'   generate labels.
#' @param score Optional column in `mutations` used for lollipop point colour,
#'   for example a BLOSUM score.
#' @param domain_start,domain_end Domain interval start/end columns.
#' @param domain Optional domain label column. When omitted, a suitable domain
#'   column is inferred from common InterProScan/domain-table names.
#' @param domain_ymin,domain_ymax Optional domain y-bound columns. If omitted,
#'   existing `ymin`/`ymax` columns are used when present.
#' @param spread_threshold Minimum x-distance between adjacent lollipop heads.
#' @param protein_start Start coordinate for the backbone rectangle.
#' @param track_ymin,track_ymax Backbone y-range.
#' @param domain_default_ymin,domain_default_ymax Default y-range for domain
#'   intervals when the interval table does not already contain y columns.
#' @param mutation_y Fixed y coordinate for lollipop heads when
#'   `mutation_y_by` is `NULL`.
#' @param mutation_y_by Optional numeric column in `mutations` used to map
#'   lollipop heads to multiple y levels, for example `"sample_count"`.
#' @param mutation_y_strategy Height-mapping strategy used when
#'   `mutation_y_by` is supplied. `"scaled"` rescales values into
#'   `mutation_y_range`; `"bins"` assigns values to tiered y levels.
#' @param mutation_y_range Numeric length-2 y range used by non-fixed height
#'   strategies.
#' @param mutation_y_trans Transformation applied before height mapping. One
#'   of `"identity"`, `"log10"`, `"log2"`, `"log"`, `"sqrt"`, or a function.
#'   Log and square-root transformations require non-negative values and logs
#'   use `value + 1`, which is convenient for count data.
#' @param mutation_y_breaks Optional numeric upper bounds for
#'   `mutation_y_strategy = "bins"`, expressed on the original
#'   `mutation_y_by` scale.
#' @param mutation_y_values Optional numeric y levels for
#'   `mutation_y_strategy = "bins"`. When `mutation_y_breaks` is supplied,
#'   this must have length `length(mutation_y_breaks) + 1`.
#' @param stem_points Number of points used per stem curve.
#' @param curve_k Sigmoid steepness for curved stems.
#'
#' @return A list with `mutations`, `domains`, `backbone`, and `stems` data
#'   frames.
#' @export
protein_lollipop_data <- function(mutations,
                                  domains = NULL,
                                  protein_length = NULL,
                                  position = "position",
                                  label = NULL,
                                  ref = NULL,
                                  alt = NULL,
                                  score = NULL,
                                  domain_start = "start",
                                  domain_end = "end",
                                  domain = NULL,
                                  domain_ymin = NULL,
                                  domain_ymax = NULL,
                                  spread_threshold = 7,
                                  protein_start = 0,
                                  track_ymin = 0.4,
                                  track_ymax = 0.6,
                                  domain_default_ymin = 0.35,
                                  domain_default_ymax = 0.65,
                                  mutation_y = 1,
                                  mutation_y_by = NULL,
                                  mutation_y_strategy = c("scaled", "bins"),
                                  mutation_y_range = c(0.85, 1.45),
                                  mutation_y_trans = "identity",
                                  mutation_y_breaks = NULL,
                                  mutation_y_values = NULL,
                                  stem_points = 30,
                                  curve_k = 1) {
  mut_df <- .lollipop_as_data_frame(mutations, "mutations")
  .lollipop_check_column(mut_df, position, "position")

  positions <- suppressWarnings(as.numeric(mut_df[[position]]))
  if (anyNA(positions)) {
    stop("`position` must identify a numeric mutation-position column.", call. = FALSE)
  }

  domain_df <- .prepare_lollipop_domain_df(
    domains = domains,
    domain_start = domain_start,
    domain_end = domain_end,
    domain = domain,
    domain_ymin = domain_ymin,
    domain_ymax = domain_ymax,
    default_ymin = domain_default_ymin,
    default_ymax = domain_default_ymax
  )
  track_ymin <- .lollipop_scalar_number(track_ymin, "track_ymin")
  track_ymax <- .lollipop_scalar_number(track_ymax, "track_ymax")
  mutation_y_values_out <- .lollipop_mutation_y_values(
    mut_df = mut_df,
    mutation_y = mutation_y,
    mutation_y_by = mutation_y_by,
    mutation_y_strategy = mutation_y_strategy,
    mutation_y_range = mutation_y_range,
    mutation_y_trans = mutation_y_trans,
    mutation_y_breaks = mutation_y_breaks,
    mutation_y_values = mutation_y_values
  )

  label_values <- .lollipop_label_values(
    mut_df = mut_df,
    positions = positions,
    label = label,
    ref = ref,
    alt = alt
  )

  mut_df$lollipop_position <- positions
  mut_df$lollipop_x <- .spread_lollipop_positions(positions, spread_threshold)
  mut_df$lollipop_y <- mutation_y_values_out
  mut_df$lollipop_label <- label_values
  mut_df$lollipop_stem_y <- rep(track_ymax, length(positions))
  mut_df$lollipop_domain <- rep(NA_character_, length(positions))

  if (!is.null(score)) {
    .lollipop_check_column(mut_df, score, "score")
    mut_df$lollipop_score <- mut_df[[score]]
  }

  if (nrow(domain_df) > 0L) {
    domain_hits <- .lollipop_domain_hits(positions, domain_df)
    hit_y <- vapply(domain_hits, function(hit) {
      if (length(hit) == 0L) {
        return(NA_real_)
      }
      max(domain_df$lollipop_ymax[hit], na.rm = TRUE)
    }, numeric(1))
    hit_domain <- vapply(domain_hits, function(hit) {
      if (length(hit) == 0L) {
        return(NA_character_)
      }
      paste(unique(as.character(domain_df$lollipop_domain[hit])), collapse = ";")
    }, character(1))

    has_hit_y <- is.finite(hit_y)
    mut_df$lollipop_stem_y[has_hit_y] <- hit_y[has_hit_y]
    mut_df$lollipop_domain[!is.na(hit_domain) & nzchar(hit_domain)] <-
      hit_domain[!is.na(hit_domain) & nzchar(hit_domain)]
  }

  if (is.null(protein_length)) {
    protein_length <- .infer_lollipop_track_length(
      positions = positions,
      domain_df = domain_df
    )
  }
  protein_length <- .lollipop_scalar_number(protein_length, "protein_length")
  protein_start <- .lollipop_scalar_number(protein_start, "protein_start")

  backbone <- data.frame(
    xmin = protein_start,
    xmax = protein_length,
    ymin = track_ymin,
    ymax = track_ymax
  )

  stems <- .generate_lollipop_stems(
    x = mut_df$lollipop_position,
    y = mut_df$lollipop_stem_y,
    xend = mut_df$lollipop_x,
    yend = mut_df$lollipop_y,
    n = stem_points,
    k = curve_k
  )

  structure(
    list(
      mutations = mut_df,
      domains = domain_df,
      backbone = backbone,
      stems = stems
    ),
    class = "ggexon_lollipop_data"
  )
}

#' Draw lollipop annotations on a protein-coordinate track
#'
#' This helper returns a list of ggplot2 layers: backbone, optional domains,
#' curved stems, and mutation heads. It is designed for protein tracks
#' but only assumes that mutations and domains share the same linear coordinate
#' system.
#'
#' @inheritParams protein_lollipop_data
#' @param show_backbone,show_domains,show_stems,show_points Logical
#'   switches for layer components.
#' @param backbone_fill,backbone_colour Backbone rectangle styling.
#' @param domain_colour Domain rectangle outline colour.
#' @param stem_colour,stem_linewidth Stem styling.
#' @param point_size,point_colour Mutation point styling. `point_colour` is used
#'   only when `score` is `NULL`; otherwise point colour is mapped to
#'   `lollipop_score`.
#' @param show.legend Passed to generated ggplot2 layers.
#'
#' @return A list of ggplot2 layers.
#' @export
geom_protein_lollipop <- function(mutations,
                                  domains = NULL,
                                  protein_length = NULL,
                                  position = "position",
                                  score = NULL,
                                  domain_start = "start",
                                  domain_end = "end",
                                  domain = NULL,
                                  domain_ymin = NULL,
                                  domain_ymax = NULL,
                                  spread_threshold = 7,
                                  protein_start = 0,
                                  track_ymin = 0.4,
                                  track_ymax = 0.6,
                                  domain_default_ymin = 0.35,
                                  domain_default_ymax = 0.65,
                                  mutation_y = 1,
                                  mutation_y_by = NULL,
                                  mutation_y_strategy = c("scaled", "bins"),
                                  mutation_y_range = c(0.85, 1.45),
                                  mutation_y_trans = "identity",
                                  mutation_y_breaks = NULL,
                                  mutation_y_values = NULL,
                                  stem_points = 30,
                                  curve_k = 1,
                                  show_backbone = TRUE,
                                  show_domains = TRUE,
                                  show_stems = TRUE,
                                  show_points = TRUE,
                                  backbone_fill = "grey80",
                                  backbone_colour = NA,
                                  domain_colour = NA,
                                  stem_colour = "grey30",
                                  stem_linewidth = 0.4,
                                  point_size = 4,
                                  point_colour = "black",
                                  show.legend = NA) {
  prepared <- protein_lollipop_data(
    mutations = mutations,
    domains = domains,
    protein_length = protein_length,
    position = position,
    score = score,
    domain_start = domain_start,
    domain_end = domain_end,
    domain = domain,
    domain_ymin = domain_ymin,
    domain_ymax = domain_ymax,
    spread_threshold = spread_threshold,
    protein_start = protein_start,
    track_ymin = track_ymin,
    track_ymax = track_ymax,
    domain_default_ymin = domain_default_ymin,
    domain_default_ymax = domain_default_ymax,
    mutation_y = mutation_y,
    mutation_y_by = mutation_y_by,
    mutation_y_strategy = mutation_y_strategy,
    mutation_y_range = mutation_y_range,
    mutation_y_trans = mutation_y_trans,
    mutation_y_breaks = mutation_y_breaks,
    mutation_y_values = mutation_y_values,
    stem_points = stem_points,
    curve_k = curve_k
  )

  layers <- list()

  if (isTRUE(show_stems) && nrow(prepared$stems) > 0L) {
    layers <- c(layers, list(
      ggplot2::geom_line(
        data = prepared$stems,
        mapping = ggplot2::aes(
          x = .data$lollipop_x,
          y = .data$lollipop_y,
          group = .data$lollipop_curve_id
        ),
        colour = stem_colour,
        linewidth = stem_linewidth,
        lineend = "round",
        linejoin = "bevel",
        inherit.aes = FALSE,
        show.legend = FALSE
      )
    ))
  }

  if (isTRUE(show_backbone)) {
    layers <- c(layers, list(
      ggplot2::geom_rect(
        data = prepared$backbone,
        mapping = ggplot2::aes(
          xmin = .data$xmin,
          xmax = .data$xmax,
          ymin = .data$ymin,
          ymax = .data$ymax
        ),
        fill = backbone_fill,
        colour = backbone_colour,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
    ))
  }

  if (isTRUE(show_domains) && nrow(prepared$domains) > 0L) {
    layers <- c(layers, list(
      ggplot2::geom_rect(
        data = prepared$domains,
        mapping = ggplot2::aes(
          xmin = .data$lollipop_start,
          xmax = .data$lollipop_end,
          ymin = .data$lollipop_ymin,
          ymax = .data$lollipop_ymax,
          fill = .data$lollipop_domain
        ),
        colour = domain_colour,
        inherit.aes = FALSE,
        show.legend = show.legend
      )
    ))
  }

  if (isTRUE(show_points)) {
    point_mapping <- if ("lollipop_score" %in% names(prepared$mutations)) {
      ggplot2::aes(
        x = .data$lollipop_x,
        y = .data$lollipop_y,
        colour = .data$lollipop_score
      )
    } else {
      ggplot2::aes(
        x = .data$lollipop_x,
        y = .data$lollipop_y
      )
    }

    point_layer <- if ("lollipop_score" %in% names(prepared$mutations)) {
      ggplot2::geom_point(
        data = prepared$mutations,
        mapping = point_mapping,
        size = point_size,
        inherit.aes = FALSE,
        show.legend = show.legend
      )
    } else {
      ggplot2::geom_point(
        data = prepared$mutations,
        mapping = point_mapping,
        size = point_size,
        colour = point_colour,
        inherit.aes = FALSE,
        show.legend = show.legend
      )
    }
    layers <- c(layers, list(point_layer))
  }

  layers
}

.lollipop_as_data_frame <- function(x, arg) {
  if (is.null(x)) {
    return(data.frame())
  }
  if (methods::is(x, "SynProteinDomainAnnotation")) {
    return(as.data.frame(query_domains(x), stringsAsFactors = FALSE))
  }
  if (methods::is(x, "DataFrame")) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  if (is.data.frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  stop("`", arg, "` must be a data frame-like object.", call. = FALSE)
}

.lollipop_check_column <- function(x, col, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a single non-empty column name.", call. = FALSE)
  }
  if (!col %in% names(x)) {
    stop("Column `", col, "` was not found in `", arg, "` data.", call. = FALSE)
  }
  invisible(TRUE)
}

.lollipop_optional_column <- function(x, col, arg) {
  if (is.null(col)) {
    return(NULL)
  }
  .lollipop_check_column(x, col, arg)
  col
}

.prepare_lollipop_domain_df <- function(domains,
                                        domain_start,
                                        domain_end,
                                        domain,
                                        domain_ymin,
                                        domain_ymax,
                                        default_ymin,
                                        default_ymax) {
  domain_df <- .lollipop_as_data_frame(domains, "domains")
  if (nrow(domain_df) == 0L) {
    return(data.frame())
  }

  .lollipop_check_column(domain_df, domain_start, "domain_start")
  .lollipop_check_column(domain_df, domain_end, "domain_end")

  domain <- domain %||% .pick_lollipop_domain_column(domain_df)
  .lollipop_check_column(domain_df, domain, "domain")

  domain_ymin <- domain_ymin %||% if ("ymin" %in% names(domain_df)) "ymin" else NULL
  domain_ymax <- domain_ymax %||% if ("ymax" %in% names(domain_df)) "ymax" else NULL
  domain_ymin <- .lollipop_optional_column(domain_df, domain_ymin, "domain_ymin")
  domain_ymax <- .lollipop_optional_column(domain_df, domain_ymax, "domain_ymax")
  default_ymin <- .lollipop_scalar_number(default_ymin, "domain_default_ymin")
  default_ymax <- .lollipop_scalar_number(default_ymax, "domain_default_ymax")

  domain_df$lollipop_start <- suppressWarnings(as.numeric(domain_df[[domain_start]]))
  domain_df$lollipop_end <- suppressWarnings(as.numeric(domain_df[[domain_end]]))
  if (anyNA(domain_df$lollipop_start) || anyNA(domain_df$lollipop_end)) {
    stop("Domain interval start/end columns must be numeric.", call. = FALSE)
  }

  domain_df$lollipop_ymin <- if (is.null(domain_ymin)) {
    default_ymin
  } else {
    suppressWarnings(as.numeric(domain_df[[domain_ymin]]))
  }
  domain_df$lollipop_ymax <- if (is.null(domain_ymax)) {
    default_ymax
  } else {
    suppressWarnings(as.numeric(domain_df[[domain_ymax]]))
  }
  if (anyNA(domain_df$lollipop_ymin) || anyNA(domain_df$lollipop_ymax)) {
    stop("Domain y-bound columns must be numeric.", call. = FALSE)
  }

  domain_df$lollipop_domain <- as.character(domain_df[[domain]])
  domain_df$lollipop_domain[is.na(domain_df$lollipop_domain) | !nzchar(domain_df$lollipop_domain)] <- "domain"

  flipped <- domain_df$lollipop_start > domain_df$lollipop_end
  if (any(flipped)) {
    old_start <- domain_df$lollipop_start[flipped]
    domain_df$lollipop_start[flipped] <- domain_df$lollipop_end[flipped]
    domain_df$lollipop_end[flipped] <- old_start
  }

  domain_df <- domain_df[
    order(domain_df$lollipop_start, domain_df$lollipop_end, domain_df$lollipop_domain),
    ,
    drop = FALSE
  ]
  rownames(domain_df) <- NULL
  domain_df
}

.pick_lollipop_domain_column <- function(x) {
  candidates <- c(
    "Domain",
    "domain_name",
    "domain",
    "motif",
    "interpro_description",
    "signature_description",
    "interpro_accession",
    "signature_accession",
    "analysis",
    "Model",
    "model",
    "name"
  )
  hit <- candidates[candidates %in% names(x)]
  if (length(hit) == 0L) {
    stop(
      "Could not infer a domain label column. Supply `domain` explicitly.",
      call. = FALSE
    )
  }
  hit[[1L]]
}

.lollipop_label_values <- function(mut_df, positions, label, ref, alt) {
  if (is.null(label) && "mutation" %in% names(mut_df)) {
    label <- "mutation"
  }
  if (!is.null(label)) {
    .lollipop_check_column(mut_df, label, "label")
    return(as.character(mut_df[[label]]))
  }
  if (!is.null(ref) && !is.null(alt)) {
    .lollipop_check_column(mut_df, ref, "ref")
    .lollipop_check_column(mut_df, alt, "alt")
    return(paste0(mut_df[[ref]], positions, mut_df[[alt]]))
  }
  as.character(positions)
}

.spread_lollipop_positions <- function(positions, threshold = 7) {
  threshold <- .lollipop_scalar_number(threshold, "spread_threshold")
  if (threshold < 0) {
    stop("`spread_threshold` must be non-negative.", call. = FALSE)
  }
  if (length(positions) == 0L) {
    return(numeric())
  }
  ord <- order(positions, seq_along(positions))
  sorted <- positions[ord]
  adjusted <- sorted
  if (length(sorted) > 1L && threshold > 0) {
    for (i in 2:length(sorted)) {
      if (sorted[[i]] - adjusted[[i - 1L]] < threshold) {
        adjusted[[i]] <- adjusted[[i - 1L]] + threshold
      }
    }
  }
  out <- numeric(length(positions))
  out[ord] <- adjusted
  out
}

.lollipop_domain_hits <- function(positions, domain_df) {
  lapply(positions, function(position) {
    which(position >= domain_df$lollipop_start & position <= domain_df$lollipop_end)
  })
}

.lollipop_mutation_y_values <- function(mut_df,
                                        mutation_y = 1,
                                        mutation_y_by = NULL,
                                        mutation_y_strategy = c("scaled", "bins"),
                                        mutation_y_range = c(0.85, 1.45),
                                        mutation_y_trans = "identity",
                                        mutation_y_breaks = NULL,
                                        mutation_y_values = NULL) {
  if (nrow(mut_df) == 0L) {
    return(numeric())
  }

  if (is.null(mutation_y_by)) {
    return(rep(.lollipop_scalar_number(mutation_y, "mutation_y"), nrow(mut_df)))
  }

  .lollipop_check_column(mut_df, mutation_y_by, "mutation_y_by")
  mutation_y_strategy <- match.arg(mutation_y_strategy)
  raw_values <- suppressWarnings(as.numeric(mut_df[[mutation_y_by]]))
  if (anyNA(raw_values)) {
    stop("`mutation_y_by` must identify a numeric column.", call. = FALSE)
  }

  transformed <- .lollipop_transform_y_values(
    raw_values,
    transform = mutation_y_trans,
    arg = "mutation_y_by"
  )

  switch(
    mutation_y_strategy,
    scaled = .lollipop_rescale_y_values(
      transformed,
      y_range = mutation_y_range
    ),
    bins = .lollipop_bin_y_values(
      transformed,
      raw_breaks = mutation_y_breaks,
      y_values = mutation_y_values,
      y_range = mutation_y_range,
      transform = mutation_y_trans
    )
  )
}

.lollipop_rescale_y_values <- function(values, y_range) {
  y_range <- .lollipop_numeric_vector(y_range, "mutation_y_range", length = 2L)
  if (length(values) == 0L) {
    return(numeric())
  }
  if (!all(is.finite(values))) {
    stop("Transformed `mutation_y_by` values must be finite.", call. = FALSE)
  }

  value_range <- base::range(values)
  if (value_range[[1L]] == value_range[[2L]]) {
    return(rep(mean(y_range), length(values)))
  }
  y_range[[1L]] + (values - value_range[[1L]]) /
    (value_range[[2L]] - value_range[[1L]]) * diff(y_range)
}

.lollipop_bin_y_values <- function(values,
                                   raw_breaks = NULL,
                                   y_values = NULL,
                                   y_range = c(0.85, 1.45),
                                   transform = "identity") {
  if (!all(is.finite(values))) {
    stop("Transformed `mutation_y_by` values must be finite.", call. = FALSE)
  }

  raw_breaks <- if (is.null(raw_breaks)) {
    NULL
  } else {
    .lollipop_numeric_vector(raw_breaks, "mutation_y_breaks")
  }

  if (is.null(y_values)) {
    n_values <- if (is.null(raw_breaks)) 4L else length(raw_breaks) + 1L
    y_range <- .lollipop_numeric_vector(y_range, "mutation_y_range", length = 2L)
    y_values <- seq(
      y_range[[1L]],
      y_range[[2L]],
      length.out = n_values
    )
  } else {
    y_values <- .lollipop_numeric_vector(y_values, "mutation_y_values")
  }

  if (is.null(raw_breaks)) {
    if (length(y_values) == 1L) {
      return(rep(y_values, length(values)))
    }
    probs <- seq(0, 1, length.out = length(y_values) + 1L)
    breaks <- as.numeric(stats::quantile(
      values,
      probs = probs[-c(1L, length(probs))],
      na.rm = TRUE,
      names = FALSE,
      type = 7
    ))
  } else {
    if (length(y_values) != length(raw_breaks) + 1L) {
      stop(
        "`mutation_y_values` must have length `length(mutation_y_breaks) + 1`.",
        call. = FALSE
      )
    }
    breaks <- .lollipop_transform_y_values(
      raw_breaks,
      transform = transform,
      arg = "mutation_y_breaks"
    )
  }

  breaks <- sort(unique(breaks))
  if (length(y_values) != length(breaks) + 1L && !is.null(raw_breaks)) {
    stop("`mutation_y_breaks` must contain unique values after transformation.", call. = FALSE)
  }
  if (length(breaks) == 0L) {
    return(rep(y_values[[1L]], length(values)))
  }

  tier <- rep(length(breaks) + 1L, length(values))
  for (i in seq_along(breaks)) {
    tier[values <= breaks[[i]] & tier == length(breaks) + 1L] <- i
  }
  y_values[pmin(tier, length(y_values))]
}

.lollipop_transform_y_values <- function(values, transform = "identity", arg = "mutation_y_by") {
  if (is.function(transform)) {
    out <- transform(values)
  } else {
    if (!is.character(transform) || length(transform) != 1L || is.na(transform)) {
      stop("`mutation_y_trans` must be a single string or function.", call. = FALSE)
    }
    transform <- match.arg(transform, c("identity", "log10", "log2", "log", "sqrt"))
    if (transform %in% c("log10", "log2", "log", "sqrt") && any(values < 0, na.rm = TRUE)) {
      stop("`", arg, "` must be non-negative for `mutation_y_trans = \"", transform, "\"`.", call. = FALSE)
    }
    out <- switch(
      transform,
      identity = values,
      log10 = log10(values + 1),
      log2 = log2(values + 1),
      log = log(values + 1),
      sqrt = sqrt(values)
    )
  }

  out <- suppressWarnings(as.numeric(out))
  if (length(out) != length(values) || anyNA(out) || !all(is.finite(out))) {
    stop("`mutation_y_trans` must return finite numeric values.", call. = FALSE)
  }
  out
}

.infer_lollipop_track_length <- function(positions, domain_df) {
  candidates <- positions
  if (nrow(domain_df) > 0L) {
    candidates <- c(candidates, domain_df$lollipop_end)
    if ("protein_length" %in% names(domain_df)) {
      candidates <- c(candidates, suppressWarnings(as.numeric(domain_df$protein_length)))
    }
  }
  candidates <- candidates[is.finite(candidates)]
  if (length(candidates) == 0L) {
    stop("Cannot infer `protein_length` from empty data.", call. = FALSE)
  }
  max(candidates)
}

.generate_lollipop_stems <- function(x, y, xend, yend, n = 30, k = 1) {
  n <- as.integer(.lollipop_scalar_number(n, "stem_points"))
  if (n < 2L) {
    stop("`stem_points` must be at least 2.", call. = FALSE)
  }
  k <- .lollipop_scalar_number(k, "curve_k")
  if (length(x) == 0L) {
    return(data.frame(
      lollipop_x = numeric(),
      lollipop_y = numeric(),
      lollipop_curve_id = integer()
    ))
  }
  normalized <- seq(0, 1, length.out = n)
  if (k == 0) {
    x_weight <- normalized
  } else {
    x_weight <- 1 / (1 + exp(-k * (normalized - 0.5) * 12))
    x_weight <- (x_weight - min(x_weight)) / (max(x_weight) - min(x_weight))
  }

  out <- lapply(seq_along(x), function(i) {
    data.frame(
      lollipop_x = x[[i]] + x_weight * (xend[[i]] - x[[i]]),
      lollipop_y = y[[i]] + normalized * (yend[[i]] - y[[i]]),
      lollipop_curve_id = i
    )
  })
  do.call(rbind, out)
}

.lollipop_scalar_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be a single numeric value.", call. = FALSE)
  }
  as.numeric(x)
}

.lollipop_numeric_vector <- function(x, arg, length = NULL) {
  if (!is.numeric(x) || anyNA(x) || !all(is.finite(x))) {
    stop("`", arg, "` must be a finite numeric vector.", call. = FALSE)
  }
  if (!is.null(length) && length(x) != length) {
    stop("`", arg, "` must have length ", length, ".", call. = FALSE)
  }
  as.numeric(x)
}
