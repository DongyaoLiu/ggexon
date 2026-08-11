#' ggplot2 Class Objects Re-exported by ggexon
#'
#' ggexon re-exports ggplot2's S7 and ggproto class objects so downstream code
#' can target the exact same class definitions without vendoring local copies.
#'
#' @name ggplot2_class_reexports
#' @keywords internal
NULL

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_gg
#' @export
class_gg <- ggplot2::class_gg

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_ggproto
#' @export
class_ggproto <- ggplot2::class_ggproto

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_scale
#' @export
class_scale <- ggplot2::class_scale

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_guides
#' @export
class_guides <- ggplot2::class_guides

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_guide
#' @export
class_guide <- ggplot2::class_guide

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_coord
#' @export
class_coord <- ggplot2::class_coord

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_facet
#' @export
class_facet <- ggplot2::class_facet

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_layer
#' @export
class_layer <- ggplot2::class_layer

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_layout
#' @export
class_layout <- ggplot2::class_layout

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_scales_list
#' @export
class_scales_list <- ggplot2::class_scales_list

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_S3_gg
#' @export
class_S3_gg <- ggplot2::class_S3_gg

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_rel
#' @export
class_rel <- ggplot2::class_rel

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_zero_grob
#' @export
class_zero_grob <- ggplot2::class_zero_grob

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_waiver
#' @export
class_waiver <- ggplot2::class_waiver

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_derive
#' @export
class_derive <- ggplot2::class_derive

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_theme
#' @export
class_theme <- ggplot2::class_theme

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_labels
#' @export
class_labels <- ggplot2::class_labels

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_mapping
#' @export
class_mapping <- ggplot2::class_mapping

#' @rdname ggplot2_class_reexports
#' @importFrom ggplot2 class_ggplot
#' @export
class_ggplot <- ggplot2::class_ggplot

#' The major ggexon object
#'
#' @section Internal panel state:
#' `panel_scale_specs` stores role-keyed specifications added through
#' `scale_panel_annotation()` and `scale_panel_coverage()`.
#' `center_annotation_panels` stores the request made by
#' `center_panel_annotation()`. These are build-time implementation properties;
#' users should add the public specification objects instead of assigning the
#' properties directly.
#' @noRd
class_ggexon <- S7::new_class(
  name = "ggexon", parent = class_gg,
  properties = list(
    data    = S7::class_any,
    layers  = S7::class_list,
    cross_panel_annotations = S7::class_list,
    panel_scale_specs = S7::class_list,
    center_annotation_panels = S7::class_logical,
    genomic_tree = S7::class_any,
    genomic_x_scale = S7::class_any,
    strip_scale = S7::class_any,
    output_size = S7::class_any,
    scales  = class_scales_list,
    guides  = class_guides,
    mapping = class_mapping,
    theme   = class_theme,
    coordinates = class_coord,
    facet   = class_facet,
    layout  = class_layout,
    labels  = class_labels,
    plot_env = S7::class_environment
  ),
  constructor = function(
    data = waiver(),
    ...,
    layers = list(),
    cross_panel_annotations = list(),
    panel_scale_specs = list(),
    center_annotation_panels = FALSE,
    genomic_tree = NULL,
    genomic_x_scale = NULL,
    strip_scale = NULL,
    output_size = NULL,
    scales = NULL,
    guides = NULL,
    mapping = aes(),
    theme = NULL,
    coordinates = coord_cartesian_genomic(default = TRUE),
    facet = facet_null(),
    layout = NULL,
    labels = labs(),
    plot_env = parent.frame()
  ) {
    warn_dots_empty()
    S7::new_object(
      S7::S7_object(),
      data        = data,
      layers      = layers,
      cross_panel_annotations = cross_panel_annotations,
      panel_scale_specs = panel_scale_specs,
      center_annotation_panels = center_annotation_panels,
      genomic_tree = genomic_tree,
      genomic_x_scale = genomic_x_scale,
      strip_scale = strip_scale,
      output_size = output_size,
      scales      = scales %||% ggplot2:::scales_list(),
      guides      = guides %||% ggplot2:::guides_list(),
      mapping     = mapping,
      theme       = theme %||% ggplot2::theme(),
      coordinates = coordinates,
      facet       = facet,
      layout      = layout %||% ggproto(NULL, Layout2),
      labels      = labels,
      plot_env    = plot_env
    )
  }
)

#' The ggplot built class
#'
#' The ggplot built class is an intermediate class and represents a processed
#' ggplot object ready for rendering. It is constructed by calling
#' [`ggplot_build()`] on a [ggplot][class_ggplot] object and is not meant to be
#' instantiated directly. The class can be rendered to a gtable object by
#' calling the [`ggplot_gtable()`] function on a ggplot built class object.
#'
#' @param ... Reserved for future expansion.
#' @param data A list of plain data frames; one for each layer.
#' @param layout A Layout ggproto object.
#' @param plot A completed ggexon class object.
#'
#' @keywords internal
#' @export
class_ggexon_built <- S7::new_class(
  "ggexon_built", parent = class_gg,
  properties = list(
    data   = S7::class_list,
    layout = class_layout,
    plot   = class_ggexon
  ),
  constructor = function(..., data = NULL, layout = NULL, plot = NULL) {
    warn_dots_empty()
    if (is.null(data) || is.null(layout) || is.null(plot)) {
      cli::cli_abort(
        "The {.cls ggexon_built} class should be constructed by {.fn ggexon_build}."
      )
    }
    S7::new_object(
      S7::S7_object(),
      data = data,
      layout = layout,
      plot = plot
    )
  }
)

# Methods -----------------------------------------------------------------

#' @importFrom S7 convert
local({
  prop_classes <- class_ggexon | class_ggexon_built

  S7::method(convert, list(from = prop_classes, to = S7::class_list)) <-
    function(from, to, ...) S7::props(from)

  S7::method(convert, list(from = S7::class_list, to = prop_classes)) <-
    function(from, to, ...) rlang::inject(to(!!!from))
})
