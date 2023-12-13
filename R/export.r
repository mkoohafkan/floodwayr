#' Export Results
#'
#' Export results to a zip file.
#'
#' @inheritParams map_results
#' @param zipfile The output file path. If `NULL`, a temporary file
#'   will be created. If the file exists, it will be overwritten.
#' @return The output filepath.
#'
#' @importFrom terra writeVector
#' @importFrom zip zip
#' @importFrom utils write.table
#' @export
export_results = function(surcharge, evaluation_lines, zipfile = NULL) {

  out_dir = tempfile(pattern = "results")
  surcharge_out = tempfile(pattern = "surcharge", fileext = ".shp",
    tmpdir = out_dir)
  lines_out = tempfile(pattern = "evaluationlines", fileext = ".shp",
    tmpdir = out_dir)
  lines_csv = tempfile(pattern = "evaluation", fileext = ".csv",
    tmpdir = out_dir)

  dir.create(out_dir)
  writeVector(surcharge, surcharge_out)
  writeVector(evaluation_lines, lines_out)
  # write the byte order mark so Excel interprets the UTF8 strings correctly
  writeChar("\UFEFF", lines_csv)
  suppressWarnings(write.table(values(evaluation_lines), lines_csv,
      append = TRUE, sep = ",", eol = "\n", fileEncoding = "UTF-8",
      row.names = FALSE, col.names = TRUE))

  if (!isTRUE(nzchar(zipfile))) {
    zipfile = tempfile(pattern = "results", fileext = ".zip")
  }

  zip(zipfile, list.files(out_dir), root = out_dir)
  zipfile
}
