#' Collect CD44 builder provenance for the LASTZ and output files.
#'
#' @noRd
collect_lastz_provenance <- function(lastz_bin, human_fasta, mouse_fasta, alignment) {
  path_exists <- function(path) {
    length(path) == 1L && !is.na(path) && file.exists(path)
  }
  md5_if_exists <- function(path) {
    if (!path_exists(path)) {
      return(NA_character_)
    }
    unname(tools::md5sum(path))
  }

  paf_path <- alignment$paf_path
  list(
    lastz_binary = if (length(lastz_bin) == 1L && !is.na(lastz_bin)) {
      basename(lastz_bin)
    } else {
      NA_character_
    },
    lastz_version = if (path_exists(lastz_bin)) {
      paste(system2(lastz_bin, "--version", stdout = TRUE), collapse = " ")
    } else {
      NA_character_
    },
    lastz_command = if (is.null(alignment$command) || length(alignment$command) == 0L) {
      NA_character_
    } else {
      alignment$command
    },
    human_fasta_md5 = md5_if_exists(human_fasta),
    mouse_fasta_md5 = md5_if_exists(mouse_fasta),
    paf_md5 = md5_if_exists(paf_path)
  )
}
