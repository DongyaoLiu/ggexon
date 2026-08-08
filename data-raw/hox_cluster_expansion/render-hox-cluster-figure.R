#!/usr/bin/env Rscript

# Render the README image from the pinned, compact HOX tutorial tables.

find_repo_root <- function(path = getwd()) {
  path <- normalizePath(path, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION")) &&
        dir.exists(file.path(path, "R"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not find the ggexon repository root.", call. = FALSE)
    }
    path <- parent
  }
}

repo_root <- find_repo_root()
devtools::load_all(repo_root, quiet = TRUE)

demo_dir <- file.path(repo_root, "inst", "extdata", "hox_cluster_expansion")
genes <- read.delim(file.path(demo_dir, "hox_genes.tsv"), check.names = FALSE)
panels <- read.delim(
  file.path(demo_dir, "hox_clusters.tsv"),
  check.names = FALSE,
  na.strings = c("", "NA")
)

row_levels <- c(
  "human", "mouse", "chicken", "gar",
  "zebrafish_a", "zebrafish_b", "amphioxus"
)
row_labels <- c(
  human = "Human",
  mouse = "Mouse",
  chicken = "Chicken",
  gar = "Spotted gar",
  zebrafish_a = "Zebrafish a",
  zebrafish_b = "Zebrafish b",
  amphioxus = "Amphioxus\n(ancestral)"
)
column_levels <- c("A", "B", "C", "D")
column_labels <- c(
  A = "HOXA",
  B = "HOXB",
  C = "HOXC",
  D = "HOXD"
)
slot_order <- paste0("Hox", 15:1)

genes$species_row <- factor(genes$species_row, levels = row_levels)
genes$cluster_column <- factor(genes$cluster_column, levels = column_levels)
genes$track <- paste(genes$species_row, genes$cluster_column, sep = "::")
genes$slot <- factor(genes$slot, levels = slot_order)
genes$y <- 1
panels$species_row <- factor(panels$matrix_row, levels = row_levels)
panels$cluster_column <- factor(panels$matrix_column, levels = column_levels)

active_panels <- panels[
  panels$cell_status != "structural_blank",
  c("species_row", "cluster_column")
]
slot_guides <- merge(
  active_panels,
  data.frame(xintercept = seq_along(slot_order)),
  by = NULL,
  all = TRUE
)
structural_panels <- panels[panels$cell_status == "structural_blank", ]
lost_panels <- panels[panels$cell_status == "cluster_not_retained", ]
hox_palette <- setNames(
  grDevices::hcl.colors(length(slot_order), "viridis"),
  slot_order
)

plot <- ggexon() +
  ggplot2::geom_rect(
    data = structural_panels,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    inherit.aes = FALSE,
    fill = "#F4F4F4",
    colour = NA
  ) +
  ggplot2::geom_rect(
    data = lost_panels,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    inherit.aes = FALSE,
    fill = "#FBE9E7",
    colour = "#C95F54",
    linewidth = 0.35,
    linetype = 2
  ) +
  ggplot2::geom_vline(
    data = slot_guides,
    ggplot2::aes(xintercept = xintercept),
    inherit.aes = FALSE,
    colour = "#D9D9D9",
    linewidth = 0.2
  ) +
  geom_genebox(
    data = genes,
    ggplot2::aes(fill = slot),
    box_size = 2.8,
    colour = "grey20",
    linewidth = 0.25,
    show.legend = FALSE
  ) +
  ggplot2::geom_text(
    data = structural_panels,
    x = 8,
    y = 1,
    label = "not applicable",
    inherit.aes = FALSE,
    colour = "#6F6F6F",
    size = 2.2
  ) +
  ggplot2::geom_text(
    data = lost_panels,
    x = 8,
    y = 1,
    label = "HOXDB not retained",
    inherit.aes = FALSE,
    colour = "#A33F36",
    size = 2.2
  ) +
  strip_scale_x(slot_order = slot_order, guide = "none") +
  ggplot2::facet_grid(
    rows = ggplot2::vars(species_row),
    cols = ggplot2::vars(cluster_column),
    scales = "fixed",
    drop = FALSE,
    switch = "y",
    labeller = ggplot2::labeller(
      species_row = ggplot2::as_labeller(row_labels),
      cluster_column = ggplot2::as_labeller(column_labels)
    )
  ) +
  ggplot2::scale_fill_manual(values = hox_palette, drop = FALSE) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(slot_order),
    labels = 15:1,
    expand = ggplot2::expansion(mult = 0)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 2),
    expand = ggplot2::expansion(mult = 0)
  ) +
  ggplot2::labs(
    x = "Hox paralog slot (posterior to anterior)",
    y = NULL,
    caption = paste0(
      "Box position = Hox paralog slot; arrow = transcription direction. ",
      "The ancestral amphioxus cluster is shown under HOXA for layout only."
    )
  ) +
  ggplot2::theme_minimal(base_size = 8) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(
      colour = "#D0D0D0", fill = NA, linewidth = 0.25
    ),
    panel.spacing = grid::unit(0.45, "lines"),
    axis.text.x = ggplot2::element_text(size = 6.5),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(
      fill = "#ECECEC", colour = "#C8C8C8", linewidth = 0.25
    ),
    strip.text.x = ggplot2::element_text(face = "bold", size = 8),
    strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, size = 7.5),
    strip.placement = "outside",
    legend.position = "none",
    plot.caption = ggplot2::element_text(size = 6.5, colour = "#555555", hjust = 0),
    plot.margin = ggplot2::margin(5.5, 8, 5.5, 5.5)
  )

output <- file.path(repo_root, "man", "figures", "hox-cluster-expansion-demo.png")
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(
  output,
  plot = plot,
  width = 10.5,
  height = 7.4,
  units = "in",
  dpi = 220,
  bg = "white"
)
message("Wrote ", output)
