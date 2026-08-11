test_that("panel scale constructors record exact role and policy", {
  ann <- scale_panel_annotation()
  cov_default <- scale_panel_coverage()
  cov <- scale_panel_coverage("free_y")

  expect_s3_class(ann, "ggexon_panel_scale_spec")
  expect_s3_class(cov_default, "ggexon_panel_scale_spec")
  expect_s3_class(cov, "ggexon_panel_scale_spec")
  expect_identical(ann[c("role", "policy")],
                   list(role = "annotation", policy = "fixed_y"))
  expect_identical(cov_default[c("role", "policy")],
                   list(role = "coverage", policy = "fixed_y"))
  expect_identical(cov[c("role", "policy")],
                   list(role = "coverage", policy = "free_y"))
})

test_that("panel scale policies are validated exactly", {
  bad <- list(
    NULL, NA_character_, character(), c("fixed_y", "free_y"),
    "free_x", "fixed", 1
  )

  for (value in bad) {
    expect_error(scale_panel_annotation(value), "policy.*fixed_y.*free_y")
    expect_error(scale_panel_coverage(value), "policy.*fixed_y.*free_y")
  }
})

test_that("panel specifications are order independent and role keyed", {
  before <- ggexon() +
    scale_panel_coverage("free_y") +
    facet_genomics(ggplot2::vars(track))
  after <- ggexon() +
    facet_genomics(ggplot2::vars(track)) +
    scale_panel_coverage("free_y")

  expect_identical(before@panel_scale_specs, after@panel_scale_specs)

  center_before <- ggexon() +
    center_panel_annotation() +
    facet_genomics(ggplot2::vars(track))
  center_after <- ggexon() +
    facet_genomics(ggplot2::vars(track)) +
    center_panel_annotation()
  expect_identical(
    center_before@center_annotation_panels,
    center_after@center_annotation_panels
  )

  both <- ggexon() +
    scale_panel_annotation("free_y") +
    scale_panel_coverage("fixed_y")
  expect_identical(both@panel_scale_specs$annotation$policy, "free_y")
  expect_identical(both@panel_scale_specs$coverage$policy, "fixed_y")
})

test_that("same-role specifications replace and centering is idempotent", {
  p <- ggexon() + scale_panel_coverage("free_y")
  expect_warning(
    replaced <- p + scale_panel_coverage("fixed_y"),
    "Replacing.*coverage"
  )
  expect_identical(replaced@panel_scale_specs$coverage$policy, "fixed_y")

  centered <- ggexon() + center_panel_annotation() + center_panel_annotation()
  expect_true(centered@center_annotation_panels)
})

test_that("panel specifications require a ggexon plot", {
  expect_error(
    ggplot2::ggplot() + scale_panel_annotation(),
    "ggexon plot"
  )
  expect_error(
    ggplot2::ggplot() + center_panel_annotation(),
    "ggexon plot"
  )
})

test_that("annotation scale policies preserve annotation-only facet geometry", {
  annotation <- data.frame(
    track = c("ann_a", "ann_b", "ann_c", "ann_d"),
    xmin = c(100, 200, 300, 400),
    xmax = c(140, 240, 340, 440),
    y = 1,
    strand = "+",
    gene = c("gene_a", "gene_b", "gene_c", "gene_d"),
    label = c("gene_a", "gene_b", "gene_c", "gene_d"),
    stringsAsFactors = FALSE
  )
  base_plot <- ggexon(SynSpecies(name = "annotation grid")) +
    geom_genetag(data = annotation, show_label = FALSE) +
    facet_genomics(ggplot2::vars(track), ncol = 2)

  layouts <- lapply(
    list(
      fixed = base_plot + scale_panel_annotation(),
      free = base_plot + scale_panel_annotation("free_y")
    ),
    function(plot) as.data.frame(ggexon_build(plot)@layout$layout)
  )
  expected_positions <- data.frame(
    ROW = c(1L, 1L, 2L, 2L),
    COL = c(1L, 2L, 1L, 2L)
  )

  for (layout in layouts) {
    expect_identical(
      layout[c("ROW", "COL")],
      expected_positions
    )
  }
  expect_length(unique(layouts$fixed$SCALE_Y), 1L)
  expect_length(unique(layouts$free$SCALE_Y), 4L)
})

test_that("annotation scale policies apply to annotation-only SynIndividual plots", {
  annotation <- data.frame(
    track = c("genes_a", "genes_b"),
    xmin = c(100, 200),
    xmax = c(140, 240),
    y = 1,
    strand = "+",
    gene = c("gene_a", "gene_b"),
    label = c("gene_a", "gene_b"),
    stringsAsFactors = FALSE
  )
  built <- ggexon_build(
    ggexon(SynIndividual(id = "sample")) +
      geom_genetag(data = annotation, show_label = FALSE) +
      facet_genomics(ggplot2::vars(track), ncol = 2) +
      scale_panel_annotation("free_y")
  )
  layout <- as.data.frame(built@layout$layout)

  expect_identical(as.character(layout$track), c("genes_a", "genes_b"))
  expect_identical(as.character(layout$panel_type), c("annotation", "annotation"))
  expect_identical(layout$ROW, c(1L, 1L))
  expect_identical(layout$COL, c(1L, 2L))
  expect_length(unique(layout$SCALE_Y), 2L)
})

test_that("annotation centering wrapper ignores ordinary facet panels", {
  ordinary <- data.frame(
    track = "ordinary",
    x = 1,
    y = 1,
    ymin = 0,
    ymax = 10
  )
  base_plot <- ggexon(ordinary) +
    ggplot2::geom_pointrange(
      ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax)
    ) +
    facet_genomics(ggplot2::vars(track))

  base_range <- ggexon_build(base_plot)@layout$panel_params[[1L]]$y.range
  wrapper_range <- ggexon_build(
    base_plot + center_panel_annotation()
  )@layout$panel_params[[1L]]$y.range
  legacy_range <- ggexon_build(
    base_plot + facet_genomics(
      ggplot2::vars(track),
      vertical = "center"
    )
  )@layout$panel_params[[1L]]$y.range

  expect_identical(wrapper_range, base_range)
  expect_false(identical(legacy_range, base_range))
})

test_that("panel ggplot_add methods match the generic signature", {
  panel_method <- getS3method("ggplot_add", "ggexon_panel_scale_spec")
  center_method <- getS3method("ggplot_add", "ggexon_annotation_center_spec")

  expect_named(formals(panel_method), c("object", "plot", "..."))
  expect_named(formals(center_method), c("object", "plot", "..."))
})

role_scale_fixture_layout <- function() {
  data.frame(
    PANEL = 1:5,
    ROW = 1:5,
    COL = 1L,
    track = c("ann_a", "cov_a", "link_a_b", "ann_b", "cov_b"),
    panel_type = c(
      "annotation", "coverage", "link", "annotation", "coverage"
    ),
    individual = c("species_a", "species_a", NA, "species_b", "species_b"),
    species = c("ann_a", "cov_a", NA, "ann_b", "cov_b"),
    alignment_name = c(NA, NA, "a_b", NA, NA),
    tspecies = c(NA, NA, "ann_b", NA, NA),
    qspecies = c(NA, NA, "ann_a", NA, NA),
    stringsAsFactors = FALSE
  )
}

legacy_pairwise_scale_fixture <- function() {
  data.frame(
    PANEL = 1:3,
    ROW = 1:3,
    COL = 1L,
    track = c("ann_a", "link_a_b", "ann_b"),
    panel_type = c("annotation", "link", "annotation"),
    species = c("ann_a", NA, "ann_b"),
    alignment_name = c(NA, "a_b", NA),
    tspecies = c(NA, "ann_b", NA),
    qspecies = c(NA, "ann_a", NA),
    stringsAsFactors = FALSE
  )
}

expect_role_scale_policy <- function(layout, role, policy, info = NULL) {
  ids <- layout$SCALE_Y[layout$panel_type == role]
  if (identical(policy, "fixed_y")) {
    expect_length(unique(ids), 1L)
  } else {
    expect_length(unique(ids), length(ids))
  }
}

panel_scale_specs <- function(annotation = NULL, coverage = NULL) {
  specs <- list()
  if (!is.null(annotation)) {
    specs$annotation <- scale_panel_annotation(annotation)
  }
  if (!is.null(coverage)) {
    specs$coverage <- scale_panel_coverage(coverage)
  }
  specs
}

test_that("mixed Syn roles allocate dense non-overlapping y-scale families", {
  cases <- list(
    list(
      annotation = NULL, coverage = NULL,
      free = list(x = FALSE, y = FALSE),
      annotation_policy = "fixed_y", coverage_policy = "fixed_y",
      link_policy = "fixed_y", effective_free_y = FALSE,
      label = "no wrappers with fixed facets"
    ),
    list(
      annotation = NULL, coverage = NULL,
      free = list(x = TRUE, y = FALSE),
      annotation_policy = "fixed_y", coverage_policy = "fixed_y",
      link_policy = "fixed_y", effective_free_y = FALSE,
      label = "no wrappers with free-x facets"
    ),
    list(
      annotation = NULL, coverage = NULL,
      free = list(x = FALSE, y = TRUE),
      annotation_policy = "fixed_y", coverage_policy = "free_y",
      link_policy = "free_y", effective_free_y = TRUE,
      label = "no wrappers with free-y facets"
    ),
    list(
      annotation = NULL, coverage = NULL,
      free = list(x = TRUE, y = TRUE),
      annotation_policy = "fixed_y", coverage_policy = "free_y",
      link_policy = "free_y", effective_free_y = TRUE,
      label = "no wrappers with free facets"
    ),
    list(
      annotation = NULL, coverage = "fixed_y",
      free = list(x = FALSE, y = TRUE),
      annotation_policy = "fixed_y", coverage_policy = "fixed_y",
      link_policy = "free_y", effective_free_y = TRUE,
      label = "fixed coverage overrides free-y facets"
    ),
    list(
      annotation = NULL, coverage = "fixed_y",
      free = list(x = TRUE, y = TRUE),
      annotation_policy = "fixed_y", coverage_policy = "fixed_y",
      link_policy = "free_y", effective_free_y = TRUE,
      label = "fixed coverage overrides free facets"
    ),
    list(
      annotation = "fixed_y", coverage = "free_y",
      free = list(x = FALSE, y = FALSE),
      annotation_policy = "fixed_y", coverage_policy = "free_y",
      link_policy = "fixed_y", effective_free_y = TRUE,
      label = "free coverage overrides fixed facets"
    ),
    list(
      annotation = "free_y", coverage = "fixed_y",
      free = list(x = FALSE, y = TRUE),
      annotation_policy = "free_y", coverage_policy = "fixed_y",
      link_policy = "free_y", effective_free_y = TRUE,
      label = "role wrappers override free-y facets independently"
    )
  )

  for (case in cases) {
    finalized <- .finalize_synspecies_layout_scales(
      role_scale_fixture_layout(),
      free = case$free,
      layout_type = "chain",
      panel_scale_specs = panel_scale_specs(
        annotation = case$annotation,
        coverage = case$coverage
      )
    )
    layout <- syn_layout_panels(finalized)

    expect_role_scale_policy(
      layout, "annotation", case$annotation_policy, info = case$label
    )
    expect_role_scale_policy(
      layout, "coverage", case$coverage_policy, info = case$label
    )

    annotation_ids <- layout$SCALE_Y[layout$panel_type == "annotation"]
    coverage_ids <- layout$SCALE_Y[layout$panel_type == "coverage"]
    link_ids <- layout$SCALE_Y[layout$panel_type == "link"]
    expect_false(any(annotation_ids %in% coverage_ids), info = case$label)
    expect_false(any(link_ids %in% coverage_ids), info = case$label)
    expect_false(any(annotation_ids %in% link_ids), info = case$label)

    all_ids <- sort(unique(layout$SCALE_Y))
    expect_identical(all_ids, seq_along(all_ids), info = case$label)

    policies <- finalized@metadata$panel_role_y_policies
    expect_identical(
      unname(unlist(policies[c("annotation", "coverage", "link")])),
      c(
        case$annotation_policy,
        case$coverage_policy,
        case$link_policy
      ),
      info = case$label
    )
    expect_identical(finalized@free$y, case$effective_free_y, info = case$label)
    expect_identical(
      infer_syn_layout_free(layout, policies)$y,
      case$effective_free_y,
      info = case$label
    )
  }
})

test_that("coverage-free pairwise y-scale identities retain their exact contract", {
  fixed <- .finalize_synspecies_layout_scales(
    legacy_pairwise_scale_fixture(),
    free = list(x = FALSE, y = FALSE),
    layout_type = "chain"
  )
  free_y <- .finalize_synspecies_layout_scales(
    legacy_pairwise_scale_fixture(),
    free = list(x = FALSE, y = TRUE),
    layout_type = "chain"
  )
  fixed_layout <- syn_layout_panels(fixed)
  free_layout <- syn_layout_panels(free_y)

  expect_identical(fixed_layout$SCALE_Y, c(1L, 1L, 1L))
  expect_identical(free_layout$SCALE_Y, c(1L, 2L, 1L))
  expect_identical(fixed_layout$t_panel, c(NA_integer_, 3L, NA_integer_))
  expect_identical(fixed_layout$q_panel, c(NA_integer_, 1L, NA_integer_))
  expect_identical(free_layout$t_panel, fixed_layout$t_panel)
  expect_identical(free_layout$q_panel, fixed_layout$q_panel)

  expect_false(fixed@free$y)
  expect_true(free_y@free$y)
  expect_false(
    infer_syn_layout_free(
      fixed_layout,
      fixed@metadata$panel_role_y_policies
    )$y
  )
  expect_true(
    infer_syn_layout_free(
      free_layout,
      free_y@metadata$panel_role_y_policies
    )$y
  )
  expect_false(infer_syn_layout_free(fixed_layout)$y)
  expect_true(infer_syn_layout_free(free_layout)$y)
})

test_that("annotation wrappers act without coverage and unused coverage specs are no-ops", {
  annotation_only <- legacy_pairwise_scale_fixture()[c(1L, 3L), ]
  annotation_only$PANEL <- 1:2
  annotation_only$ROW <- 1:2

  annotation_free <- .finalize_synspecies_layout_scales(
    annotation_only,
    free = list(x = FALSE, y = FALSE),
    panel_scale_specs = panel_scale_specs(annotation = "free_y")
  )
  unused_coverage <- .finalize_synspecies_layout_scales(
    annotation_only,
    free = list(x = FALSE, y = FALSE),
    panel_scale_specs = panel_scale_specs(coverage = "free_y")
  )

  expect_identical(syn_layout_panels(annotation_free)$SCALE_Y, c(1L, 2L))
  expect_true(annotation_free@free$y)
  expect_identical(syn_layout_panels(unused_coverage)$SCALE_Y, c(1L, 1L))
  expect_false(unused_coverage@free$y)
  expect_identical(
    unused_coverage@metadata$panel_role_y_policies,
    list(annotation = "fixed_y")
  )
})

test_that("role-scale finalization preserves stored multi-column geometry", {
  panels <- data.frame(
    PANEL = 1:4,
    ROW = c(1L, 1L, 2L, 2L),
    COL = c(1L, 2L, 1L, 2L),
    track = c("ann_a", "ann_b", "ann_c", "ann_d"),
    panel_type = "annotation",
    species = c("ann_a", "ann_b", "ann_c", "ann_d"),
    stringsAsFactors = FALSE
  )

  finalized <- syn_layout_panels(.finalize_synspecies_layout_scales(
    panels,
    free = list(x = FALSE, y = FALSE),
    panel_scale_specs = panel_scale_specs(annotation = "free_y")
  ))

  expect_identical(finalized$PANEL, 1:4)
  expect_identical(finalized$ROW, c(1L, 1L, 2L, 2L))
  expect_identical(finalized$COL, c(1L, 2L, 1L, 2L))
  expect_identical(finalized$SCALE_Y, 1:4)
})

test_that("prepended coverage rows preserve stored multi-column geometry", {
  annotation_panels <- data.frame(
    PANEL = 1:4,
    ROW = c(1L, 1L, 2L, 2L),
    COL = c(1L, 2L, 1L, 2L),
    track = c("ann_a", "ann_b", "ann_c", "ann_d"),
    panel_type = "annotation",
    individual = c("ann_a", "ann_b", "ann_c", "ann_d"),
    species = c("ann_a", "ann_b", "ann_c", "ann_d"),
    xlim_chr = "chr1",
    xlim_min = 100,
    xlim_max = 200,
    stringsAsFactors = FALSE
  )
  with_coverage <- .prepend_synspecies_coverage_rows(
    SynLayout(
      panels = annotation_panels,
      free = list(x = TRUE, y = FALSE)
    ),
    coverage_tracks = "ann_a",
    coverage_windows = list(ann_a = list(
      chr = "chr1", start = 100, end = 200, individual = "ann_a"
    ))
  )

  finalized <- syn_layout_panels(.finalize_synspecies_layout_scales(
    with_coverage,
    free = list(x = TRUE, y = FALSE)
  ))

  expect_identical(finalized$panel_type, c("coverage", rep("annotation", 4L)))
  expect_identical(finalized$ROW, c(1L, 2L, 2L, 3L, 3L))
  expect_identical(finalized$COL, c(1L, 1L, 2L, 1L, 2L))
})

test_that("panel-limit re-finalization keeps the facet fallback for link policy", {
  specs <- panel_scale_specs(coverage = "free_y")
  initial <- .finalize_synspecies_layout_scales(
    role_scale_fixture_layout(),
    free = list(x = FALSE, y = FALSE),
    layout_type = "chain",
    panel_scale_specs = specs
  )
  annotation_path <- system.file(
    "extdata",
    "caenorhabditis_XZ1516.gff3",
    package = "ggexon"
  )
  species <- SynSpecies(name = "panel-limit policies") |>
    add_individual(
      test_syn_individual(annotation_file = annotation_path, id = "ann_a"),
      test_syn_individual(annotation_file = annotation_path, id = "ann_b")
    )
  species_layout(species) <- initial
  params <- list(
    panel_xlim = list(ann_a = c(10, 20), ann_b = c(30, 40)),
    panel_xlim_chr = list(ann_a = "RagTag_V", ann_b = "RagTag_V"),
    free = list(x = FALSE, y = FALSE),
    panel_scale_specs = specs
  )

  updated <- .apply_facet_panel_xlim_to_layout(
    initial,
    plot_data = species,
    params = params
  )

  expect_identical(
    updated@metadata$panel_role_y_policies,
    list(annotation = "fixed_y", coverage = "free_y", link = "fixed_y")
  )
  expect_true(updated@free$x)
  expect_true(updated@free$y)
})

test_that("legacy inference separates role identity from within-role freedom", {
  fixed_mixed <- role_scale_fixture_layout()
  fixed_mixed$SCALE_X <- 1L
  fixed_mixed$SCALE_Y <- c(1L, 2L, 3L, 1L, 2L)

  free_mixed <- fixed_mixed
  free_mixed$SCALE_Y <- c(1L, 2L, 4L, 1L, 3L)

  expect_false(infer_syn_layout_free(fixed_mixed)$y)
  expect_true(infer_syn_layout_free(free_mixed)$y)
  expect_false(as_syn_layout(fixed_mixed)@free$y)
  expect_true(as_syn_layout(free_mixed)@free$y)

  metadata_fixed <- as_syn_layout(
    free_mixed,
    metadata = list(panel_role_y_policies = list(
      annotation = "fixed_y",
      coverage = "fixed_y",
      link = "fixed_y"
    ))
  )
  metadata_free <- as_syn_layout(
    fixed_mixed,
    metadata = list(panel_role_y_policies = list(
      annotation = "fixed_y",
      coverage = "free_y",
      link = "fixed_y"
    ))
  )
  expect_false(metadata_fixed@free$y)
  expect_true(metadata_free@free$y)

  annotation_only <- legacy_pairwise_scale_fixture()[c(1L, 3L), ]
  annotation_only$SCALE_X <- 1L
  annotation_only$SCALE_Y <- 1L
  expect_false(
    infer_syn_layout_free(
      annotation_only,
      list(coverage = "free_y")
    )$y
  )
})
