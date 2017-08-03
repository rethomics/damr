#' Get path to damr example
#'
#' damr comes with a sample DAM2 files in its `inst/extdata`
#' directory. `damr_example` allow  make them easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @author Hadley Wickham  (modified from readr)
#' @export
#' @examples
#' # list all files
#' damr_example()
#' # get path to one file
#' damr_example("M014.txt")
#' # get the directory wih all the files
#' damr_example_dir()
damr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "damr"))
  } else {
    system.file("extdata", path, package = "damr", mustWork = TRUE)
  }
}


#' @rdname damr_example
#' @export
damr_example_dir <- function() {
  system.file("extdata",package = "damr")
}

