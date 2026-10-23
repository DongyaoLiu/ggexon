# Example-local signal layer. Explicitly participates in gene annotation panels.
# Raw values and native closed interval coordinates survive ggplot's build for QC.
GeomAnnotationSignal <- ggplot2::ggproto(
  "GeomAnnotationSignal", ggplot2::GeomRect,
  ggexon_panel_role = "annotation",
  required_aes = c("xmin", "xmax", "ymin", "ymax", "track", "raw_score",
                   "interval_start", "interval_end")
)
annotation_signal_layer <- function(data, fill) {
  ggplot2::layer(
    data=data,
    mapping=ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
      track=track, raw_score=raw_score, interval_start=interval_start,
      interval_end=interval_end),
    geom=GeomAnnotationSignal, stat="identity", position="identity",
    inherit.aes=FALSE, params=list(fill=fill, colour=NA, na.rm=FALSE)
  )
}

prepare_signal_lane <- function(data, baseline, height, limits) {
  stopifnot(nrow(data)>0, all(is.finite(data$coverage)),
    all(data$coverage>=limits[1] & data$coverage<=limits[2]),
    limits[2]>limits[1], height>0, baseline+height<0)
  # Affine display mapping into a labelled lane, not normalization of source scores.
  data$raw_score <- data$coverage
  data$interval_start <- data$xmin
  data$interval_end <- data$xmax
  data$xmin <- data$interval_start-0.5
  data$xmax <- data$interval_end+0.5
  data$ymin <- baseline
  data$ymax <- baseline+height*(data$raw_score-limits[1])/diff(limits)
  data
}
