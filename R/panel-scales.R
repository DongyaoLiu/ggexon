#' Set the annotation-panel y-scale policy
#'
#' `scale_panel_annotation()` controls how first-class annotation panels inherit
#' y-scale objects in a Syn-aware [`facet_genomics()`] layout. With the default
#' `"fixed_y"`, all annotation panels share one scale object. With `"free_y"`,
#' each annotation panel receives its own scale object.
#'
#' An explicit annotation policy takes precedence over the y component of the
#' facet's `scales` argument. A valid specification is a no-op when no
#' annotation panel is present. Ordinary non-Syn facets and
#' [`facet_genomictree()`] retain their existing behavior.
#'
#' @param policy One non-missing string, exactly `"fixed_y"` (the default) or
#'   `"free_y"`.
#'
#' @return An object of class `ggexon_panel_scale_spec` that can be added to a
#'   ggexon plot.
#' @seealso [`scale_panel_coverage()`], [`center_panel_annotation()`],
#'   [`facet_genomics()`]
#' @examples
#' annotation_scales <- ggexon() +
#'   facet_genomics(ggplot2::vars(track)) +
#'   scale_panel_annotation("free_y")
#' @export
scale_panel_annotation <- function(policy = "fixed_y") {
  .new_panel_scale_spec("annotation", policy)
}

#' Set the coverage-panel y-scale policy
#'
#' `scale_panel_coverage()` controls how first-class BigWig coverage panels
#' inherit y-scale objects. `"fixed_y"` (the default) gives all coverage panels
#' one shared raw-depth scale; `"free_y"` gives every coverage panel its own
#' raw-depth scale.
#'
#' When this wrapper is absent, the y component of
#' [`facet_genomics(scales = ...)`][facet_genomics] supplies the coverage
#' fallback: `"fixed"` and `"free_x"` mean `"fixed_y"`, while `"free_y"` and
#' `"free"` mean `"free_y"`. Annotation panels remain `"fixed_y"` by default.
#' An explicit coverage policy takes precedence over that fallback, and a valid
#' specification is a no-op when no coverage panel is present. Ordinary
#' non-Syn facets and [`facet_genomictree()`] are unchanged.
#'
#' @param policy One non-missing string, exactly `"fixed_y"` (the default) or
#'   `"free_y"`.
#'
#' @return An object of class `ggexon_panel_scale_spec` that can be added to a
#'   ggexon plot.
#' @seealso [`scale_panel_annotation()`], [`center_panel_annotation()`],
#'   [`geom_coverage()`], [`facet_genomics()`]
#' @examples
#' fixture_dir <- system.file("extdata", "peel1_coverage", package = "ggexon")
#' gtf <- file.path(
#'   fixture_dir,
#'   "WS285.ugt31-zeel1-peel1-nekl1.gtf"
#' )
#' strains <- c("XZ1516", "ECA2091", "ECA701", "ECA2191")
#'
#' coverage_species <- SynSpecies(name = "PEEL-1 coverage")
#' for (strain in strains) {
#'   individual <- SynIndividual(
#'     annotation_file = gtf,
#'     annotation_format = "gtf",
#'     id = strain
#'   )
#'   individual <- add_annotation(
#'     individual,
#'     SynBigWigAnnotation(
#'       name = "coverage",
#'       bigwig_file = file.path(fixture_dir, paste0(strain, ".raw.bw"))
#'     )
#'   )
#'   coverage_species <- add_individual(coverage_species, individual)
#' }
#'
#' coverage_layers <- ggexon(coverage_species) +
#'   geom_coverage(annotation = "coverage") +
#'   geom_exon(
#'     species = "XZ1516",
#'     chr = "I",
#'     subset = c(2332338L, 2373985L)
#'   ) +
#'   facet_genomics(
#'     ggplot2::vars(track),
#'     ncol = 1,
#'     strip.position = "left"
#'   )
#'
#' # One shared raw-depth scale across all coverage panels.
#' shared_depth <- coverage_layers + scale_panel_coverage("fixed_y")
#'
#' # One raw-depth scale per coverage panel, plus a centered gene model.
#' free_depth <- coverage_layers +
#'   scale_panel_coverage("free_y") +
#'   center_panel_annotation() +
#'   theme_ggexon_track() +
#'   theme_ggexon_side_strips("left")
#' @export
scale_panel_coverage <- function(policy = "fixed_y") {
  .new_panel_scale_spec("coverage", policy)
}

#' Center annotation panels
#'
#' `center_panel_annotation()` requests a post-training view adjustment for
#' annotation panels in a Syn-aware plot. Each annotation panel's visible y
#' range is made symmetric around its annotation bodies. Built annotation data
#' and inherited scale training are not changed, and coverage and link panel
#' ranges are left alone. Ordinary non-Syn facets are unchanged.
#'
#' This is a panel-view operation, not a ggplot2 `position_*()` adjustment. It
#' is the dedicated Syn-aware equivalent of
#' `facet_genomics(vertical = "center")`; adding both to a Syn-backed plot
#' applies the same operation once. Repeated additions are idempotent, and a
#' plot without Syn annotation panels is unchanged.
#'
#' @return An object of class `ggexon_annotation_center_spec` that can be added
#'   to a ggexon plot.
#' @seealso [`scale_panel_annotation()`], [`scale_panel_coverage()`],
#'   [`facet_genomics()`]
#' @examples
#' centered_annotation <- ggexon() +
#'   facet_genomics(ggplot2::vars(track)) +
#'   center_panel_annotation()
#' @export
center_panel_annotation <- function() {
  structure(list(), class = "ggexon_annotation_center_spec")
}

.validate_panel_scale_policy <- function(policy) {
  choices <- c("fixed_y", "free_y")
  if (!is.character(policy) || length(policy) != 1L ||
      is.na(policy) || !policy %in% choices) {
    cli::cli_abort(c(
      "{.arg policy} must be exactly one of {.val fixed_y} or {.val free_y}."
    ))
  }
  policy
}

.new_panel_scale_spec <- function(role, policy) {
  structure(
    list(role = role, policy = .validate_panel_scale_policy(policy)),
    class = "ggexon_panel_scale_spec"
  )
}

.facet_y_policy <- function(free) {
  if (isTRUE(free$y)) "free_y" else "fixed_y"
}

.resolve_panel_y_policy <- function(role, specs, free) {
  specs <- specs %||% list()
  explicit <- specs[[role]]
  if (!is.null(explicit)) {
    return(explicit$policy)
  }
  if (identical(role, "annotation")) {
    return("fixed_y")
  }
  if (identical(role, "coverage")) {
    return(.facet_y_policy(free))
  }
  NULL
}

.resolve_present_panel_y_policies <- function(panel_roles, specs, free) {
  panel_roles <- unique(as.character(panel_roles))
  panel_roles <- panel_roles[!is.na(panel_roles) & nzchar(panel_roles)]
  policies <- list()

  for (role in panel_roles) {
    policy <- if (identical(role, "link")) {
      .facet_y_policy(free)
    } else {
      .resolve_panel_y_policy(role, specs, free)
    }
    if (!is.null(policy)) {
      policies[[role]] <- policy
    }
  }
  policies
}

#' @export
ggplot_add.ggexon_panel_scale_spec <- function(object, plot, ...) {
  if (!is_ggexon(plot)) {
    cli::cli_abort("Panel scale specifications can only be added to a ggexon plot.")
  }

  role <- object$role
  if (!is.null(plot@panel_scale_specs[[role]])) {
    cli::cli_warn("Replacing the existing {.val {role}} panel scale specification.")
  }
  plot@panel_scale_specs[[role]] <- object
  plot
}

#' @export
ggplot_add.ggexon_annotation_center_spec <- function(object, plot, ...) {
  if (!is_ggexon(plot)) {
    cli::cli_abort("Annotation centering can only be added to a ggexon plot.")
  }
  plot@center_annotation_panels <- TRUE
  plot
}
