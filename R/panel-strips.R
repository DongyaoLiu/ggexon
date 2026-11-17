# Span the existing strip grob after facet layout has resolved panel roles,
# axes, and physical heights. Reusing the grob preserves labellers and themes.
apply_individual_panel_strips <- function(table, build) {
  params <- build@plot@facet$params %||% list()
  if (!identical(params$strip_group %||% "panel", "individual")) {
    return(table)
  }
  panels <- as.data.frame(build@layout$layout)
  if (!all(c("panel_type", "ROW", "COL", "track") %in% names(panels))) {
    return(table)
  }
  side <- substr(params$strip.position, 1L, 1L)
  strip_candidates <- which(grepl(paste0("^strip-", side, "-"), table$layout$name))
  if (!length(strip_candidates)) return(table)

  individual <- rep(NA_character_, nrow(panels))
  for (field in intersect(c("individual", "species", "track"), names(panels))) {
    values <- as.character(panels[[field]])
    fill <- (is.na(individual) | !nzchar(individual)) &
      !is.na(values) & nzchar(values)
    individual[fill] <- values[fill]
  }
  eligible <- panels$panel_type %in% c("annotation", "coverage") &
    !is.na(individual) & nzchar(individual)

  groups <- list()
  for (column in unique(panels$COL)) {
    rows <- which(panels$COL == column)
    rows <- rows[order(panels$ROW[rows])]
    previous <- NA_character_
    previous_row <- NA_integer_
    group <- integer()
    for (i in rows) {
      same <- isTRUE(eligible[i]) && !is.na(previous) &&
        identical(individual[i], previous) && panels$ROW[i] == previous_row + 1L
      if (!same && length(group)) {
        groups[[length(groups) + 1L]] <- group
        group <- integer()
      }
      if (isTRUE(eligible[i])) group <- c(group, i)
      previous <- if (isTRUE(eligible[i])) individual[i] else NA_character_
      previous_row <- panels$ROW[i]
    }
    if (length(group)) groups[[length(groups) + 1L]] <- group
  }

  for (group_id in seq_along(groups)) {
    members <- groups[[group_id]]
    cells <- lapply(members, function(i) panel_gtable_index(table,
      ggexon_gtable_index(panels$COL[i]), ggexon_gtable_index(panels$ROW[i]),
      n_panels = nrow(panels)))
    if (any(lengths(cells) != 1L)) next
    cells <- unlist(cells)
    strips <- vapply(cells, function(cell) {
      candidates <- strip_candidates[
        table$layout$t[strip_candidates] == table$layout$t[cell] &
          table$layout$b[strip_candidates] == table$layout$b[cell]]
      if (side == "l") {
        candidates <- candidates[table$layout$r[candidates] < table$layout$l[cell]]
        if (length(candidates)) return(candidates[which.max(table$layout$r[candidates])])
      } else {
        candidates <- candidates[table$layout$l[candidates] > table$layout$r[cell]]
        if (length(candidates)) return(candidates[which.min(table$layout$l[candidates])])
      }
      NA_integer_
    }, integer(1))
    if (anyNA(strips) || anyDuplicated(strips)) next
    annotation <- which(panels$panel_type[members] == "annotation")
    template <- strips[if (length(annotation)) annotation[[1L]] else 1L]
    for (i in setdiff(strips, template)) table$grobs[[i]] <- zeroGrob()
    table$layout$t[template] <- min(table$layout$t[cells])
    table$layout$b[template] <- max(table$layout$b[cells])
    table$layout$name[template] <- paste0("strip-", side, "-individual-",
      panels$COL[members[[1L]]], "-", group_id)
  }
  table
}
