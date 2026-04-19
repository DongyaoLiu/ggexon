test_that("odgi_node_table exposes the bundled script path", {
  script <- odgi_node_table_script()

  expect_true(file.exists(script))
  expect_match(basename(script), "^odgi_node_table\\.py$")
})

test_that("odgi_node_table builds a node table via the bundled Python script", {
  skip_if(.Platform$OS.type == "windows")

  python <- Sys.which("python3")
  if (!nzchar(python)) {
    python <- Sys.which("python")
  }
  skip_if(!nzchar(python), "Python is required for odgi_node_table().")

  og_file <- tempfile(fileext = ".og")
  writeLines("fake graph", og_file)

  fake_odgi <- tempfile(fileext = ".py")
  writeLines(
    c(
      "#!/usr/bin/env python3",
      "import sys",
      "",
      "cmd = sys.argv[1]",
      "if cmd == 'view':",
      "    sys.stdout.write('S\\t1\\tAC\\nS\\t2\\tG\\nP\\tXZ1516#V_RagTag:100-102\\t1+,2-\\t*\\n')",
      "elif cmd == 'paths':",
      "    sys.stdout.write('XZ1516#V_RagTag:100-102\\n')",
      "elif cmd == 'position':",
      "    sys.stdout.write('path\\tnode_id\\tposition\\nXZ1516#V_RagTag:100-102\\t1\\t0\\nXZ1516#V_RagTag:100-102\\t2\\t2\\n')",
      "else:",
      "    raise SystemExit(f'Unexpected command: {cmd}')"
    ),
    fake_odgi
  )
  Sys.chmod(fake_odgi, mode = "0755")

  out_path <- odgi_node_table(
    og_file = og_file,
    odgi = fake_odgi,
    python = python,
    read = FALSE
  )

  expect_true(file.exists(out_path))

  tbl <- odgi_node_table(
    og_file = og_file,
    odgi = fake_odgi,
    python = python,
    read = TRUE
  )

  expect_s3_class(tbl, "data.frame")
  expect_identical(tbl$node_id, c(1L, 2L))
  expect_identical(tbl$sequence, c("AC", "G"))
  expect_identical(tbl$XZ1516_chromosome, c("V_RagTag", "V_RagTag"))
  expect_identical(tbl$XZ1516_strand, c("+", "-"))
  expect_identical(tbl$XZ1516_absolute_start, c(100L, 102L))
  expect_identical(tbl$XZ1516_absolute_end, c(101L, 102L))
})
