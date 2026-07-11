test_that("locus sets infer paralog windows and build panel-track grids", {
  human_gff <- tempfile(fileext = ".gff3")
  chimp_gff <- tempfile(fileext = ".gff3")

  writeLines(
    c(
      "##gff-version 3",
      "chr1\ttest\tgene\t1000\t2000\t.\t+\t.\tID=gene-NOTCH2;Name=NOTCH2;gene=NOTCH2",
      "chr1\ttest\tgene\t4300\t4700\t.\t+\t.\tID=gene-NBPF10;Name=NBPF10;gene=NBPF10",
      "chr1\ttest\tgene\t5000\t5600\t.\t+\t.\tID=gene-NOTCH2NLA;Name=NOTCH2NLA;gene=NOTCH2NLA",
      "chr1\ttest\tgene\t5900\t6300\t.\t+\t.\tID=gene-SEC22B;Name=SEC22B;gene=SEC22B"
    ),
    human_gff
  )
  writeLines(
    c(
      "##gff-version 3",
      "chr1\ttest\tgene\t1000\t2000\t.\t+\t.\tID=gene-NOTCH2;Name=NOTCH2;gene=NOTCH2",
      "chr1\ttest\tgene\t4300\t4700\t.\t+\t.\tID=gene-NBPF10;Name=NBPF10;gene=NBPF10",
      "chr1\ttest\tgene\t5900\t6300\t.\t+\t.\tID=gene-SEC22B;Name=SEC22B;gene=SEC22B"
    ),
    chimp_gff
  )

  sp <- SynSpecies(name = "notch2nl") |>
    add_individual(
      SynIndividual(annotation_file = human_gff, genome_file = genome_waiver(), id = "human") |>
        load_annotation(),
      SynIndividual(annotation_file = chimp_gff, genome_file = genome_waiver(), id = "chimp") |>
        load_annotation()
    )

  loci <- infer_locus_windows(
    sp,
    loci = c("NOTCH2", "NOTCH2NLA"),
    anchors = list(NOTCH2NLA = c("NBPF", "SEC22B")),
    flank = 1200
  )
  locus_tbl <- locus_table(loci)

  expect_s4_class(loci, "SynLocusSet")
  expect_equal(nrow(locus_tbl), 4L)
  expect_true(any(
    locus_tbl$individual == "chimp" &
      locus_tbl$col_group == "NOTCH2NLA" &
      locus_tbl$window_source == "anchor_inferred"
  ))

  sp <- sp |>
    add_locus_set(loci) |>
    use_locus_grid(
      row_order = c("human", "chimp"),
      col_order = c("NOTCH2", "NOTCH2NLA")
    )

  panels <- syn_layout_panels(species_layout(sp))
  expect_identical(
    as.character(panels$track),
    c("human__NOTCH2", "human__NOTCH2NLA", "chimp__NOTCH2", "chimp__NOTCH2NLA")
  )
  expect_identical(as.integer(panels$ROW), c(1L, 1L, 2L, 2L))
  expect_identical(as.integer(panels$COL), c(1L, 2L, 1L, 2L))
  expect_identical(get_locus_set(sp), loci)

  context <- list(windows = collect_layout_panel_windows(sp))
  gene_tags <- syn_to_genetag_df(sp, context = context)
  chimp_derived <- gene_tags[as.character(gene_tags$track) == "chimp__NOTCH2NLA", , drop = FALSE]

  expect_setequal(as.character(unique(gene_tags$track)), as.character(locus_tbl$track))
  expect_true(all(chimp_derived$individual == "chimp"))
  expect_true(any(chimp_derived$gene %in% c("NBPF10", "SEC22B")))
  expect_false(any(chimp_derived$gene == "NOTCH2NLA"))

  plot_obj <- ggexon(sp) +
    geom_genetag() +
    facet_genomics(ggplot2::vars(track), scales = "free")
  built <- ggexon_build(plot_obj)

  expect_setequal(as.character(built@layout$layout$track), as.character(locus_tbl$track))
  expect_true("chimp__NOTCH2NLA" %in% as.character(built@data[[1L]]$track))
})
