#' @keywords internal
"_PACKAGE"

## ggexon namespace imports -----------------------------------------------------
## Imports are declared selectively here so the package no longer relies on a
## broad `Depends:` search path. Symbols are grouped by their source package.

#' @importFrom dplyr %>% arrange distinct filter first group_by if_else
#'   left_join mutate n rename row_number rowwise select slice summarize ungroup
#' @importFrom rlang arg_match0 as_function as_string call_match caller_arg
#'   caller_env check_dots_empty check_installed cnd_message current_env dots_list
#'   enexprs expr get_expr inject is_bare_list is_bare_numeric is_call is_null
#'   is_quosure is_symbol new_quosure quo_get_env quo_get_expr quo_is_symbol
#'   quo_text set_names sym try_fetch format_error_call zap
#' @importFrom lifecycle deprecated
#' @importFrom glue glue
#' @importFrom reshape2 melt
#' @importFrom vctrs data_frame new_data_frame vec_cast vec_cbind vec_ptype2
#'   vec_rbind vec_unique vec_unique_count
#' @importFrom grid convertHeight convertWidth gList gTree gpar grobHeight
#'   grobName grobWidth is.grob is.unit polylineGrob segmentsGrob unit unit.c
#'   arrow drawDetails
#' @importFrom gtable gtable_add_cols gtable_add_grob gtable_add_padding
#'   gtable_add_rows
#' @importFrom scales label_ordinal
#' @import ggplot2
#' @importFrom BiocGenerics width
#' @importFrom GenomicRanges GRanges
#' @importFrom GenomeInfoDb seqnames
#' @importFrom IRanges IRanges
#' @importFrom tibble tibble rownames_to_column
#' @importFrom methods new show validObject
#' @importFrom stats ave setNames
#' @importFrom utils file_test head tail
NULL

# Non-standard-evaluation column names referenced inside dplyr/ggplot2 verbs.
# Declaring them keeps `R CMD check` quiet about "no visible binding for global
# variable" without scattering .data pronouns through the codebase.
utils::globalVariables(c(
  "Layout", "PANEL", "a_x", "a_y", "b1_x", "b1_y", "b2_x", "b2_y", "base_length",
  "data_melt", "end", "fill", "gene", "group", "gtable_height", "gtable_width",
  "height", "height2", "linetype", "linewidth", "max_xmax", "min_xmin", "pass",
  "position", "q_panel", "qchr", "qend", "qspecies", "qstart", "query_anchor_y",
  "s", "start", "start_average", "strand", "t_panel", "target_anchor_y", "tchr",
  "tend", "track", "track_name", "track_y", "track_y_m", "trackname",
  "transcripts", "transcripts_length", "tspecies", "tstart", "value", "variable",
  "x", "xend", "xmax", "xmin", "y", "y0", "y1", "y3", "y_middle", "y_range",
  "yend", "ymax", "ymin"
))

#' Display ggexon S4 objects
#'
#' Compact `show()` methods that print a one-screen summary of the major
#' `ggexon` S4 objects (`SynSpecies`, `SynIndividual`, `SynLayout`,
#' `SynAnnotation`, and `HomologyAnnotation`).
#'
#' @param object A `ggexon` S4 object.
#' @return `object`, invisibly; called for the side effect of printing a summary.
#' @name ggexon-show
#' @rdname ggexon-show
#' @keywords internal
NULL
