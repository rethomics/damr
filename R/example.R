#' Get path to damr example
#'
#' damr comes with a sample DAM2 files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @author Hadley Wickham  (copied from readr)
#' @export
#' @examples
#' damr_example()
#' damr_example("M014.txt")
damr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "damr"))
  } else {
    system.file("extdata", path, package = "damr", mustWork = TRUE)
  }
}
