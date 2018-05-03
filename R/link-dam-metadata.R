#' Link DAM2 or DAM5 metadata to result files
#'
#' This function checks and add columns to DAMS metadata.
#' This way, it can subsequently be loaded (via [load_dam]).
#'
#' @param x object such as a [data.frame], or the name of a file (see detail)
#' @param result_dir the root directory where all daily data are saved
#' @return a [data.table::data.table] with the same rows as x, and extra columns used for further data loading
#' @details
#' These function will augment metadata from two different types of inputs:
#' 1. A [data.frame] (recommended)
#' In this case, the function will try to match requested data with data available on `result_dir`.
#' The provided [data.frame] or [data.table] has typically one row per requested individual and the columns
#' (not necessarily in this order):
#'     * `file` -- the name of a data file (e.g. `"Monitor3.txt"`), it has to exists in result_dir.
#'     * `start_datetime` -- the first day **and time** of the requested experiment (e.g. `"2014-12-28 18:00:00"`).
#'     * `stop_datetime` -- the last day and time of the requested experiment (e.g. `"2014-12-30  19:00:00"` or simply `"2014-12-30"`).
#'     * `region_id` -- the channel (between 1 and 32) in which the animal was in (e.g. `20`).
#'   `region_id` is optional. If not provided, all 32 channels are loaded *with the same conditions*.
#'     * `???` *any number of arbitrary columns* to associate `conditions`/`treatments`/`genotypes`/... to the previous columns.
#'
#' 2. The name of a CSV file that contains a table as described in `1`.
#'
#'
#' The time in data is expressed relatively to start_date.
#' In other words, if you do circadian analysis, and your `D -> L` transitions are at 09:00:00,
#' you want to set `start_datetime = "YYY-MM-DD 09:00:00"`.
#' The `result_directory`` is the folder conraining all result (.txt) files
#' (for instance, `result_dir = "C:/where/I/Store/my/txt/files/"`)
#' @seealso
#' * [load_dam] -- to subsequently load the actual data
#' @references
#' * [the rethomics workflow](https://rethomics.github.io/workflow.html) -- on the concept of "linking"
#' * [metadata tutorial](https://rethomics.github.io/metadata.html) -- how to work with metadata
#' @export link_dam_metadata link_dam2_metadata
#' @aliases link_dam2_metadata
link_dam_metadata <- function(x, result_dir){
  . = id = region_id = experiment_id = start_datetime = path = NULL
  if(is.character(x))
    q <- data.table::fread(x)
  else
    q <- data.table::copy(data.table::as.data.table(x))

  check_dir_exists(result_dir)

  cn <- colnames(q)
  if(!("file" %in% cn & "start_datetime" %in% cn & "stop_datetime" %in% cn )){
    stop("metadata MUST have at least three columns names `file`, `start_datetime` and `stop_datetime`")
  }
  q[,path:=paste(result_dir,file,sep="/")]
  q[,file:=NULL]


  # force format for midnight dates (#6)
  q[, start_datetime := sapply(start_datetime, function(x) format(parse_datetime(x), format = "%F %T"))]

  q[,experiment_id := sprintf("%s|%s",
                              start_datetime,
                              basename(path))]

  # we expand query for all channels if no channel provided
  if(!"region_id" %in% cn)
    q <- q[q[,.(region_id=1:32),by=experiment_id], on="experiment_id"]

  q[,id := as.factor(sprintf("%s|%02d",
                 experiment_id,
                 region_id))]
  q[, experiment_id := NULL]

  data.table::setkeyv(q, "id")

  file_info <- q[,.(file_info =  list(list(path = path, file = basename(path)))), by="id"]
  out <- file_info[q]
  out[, path := NULL]
  out


}


link_dam2_metadata <- function(x, result_dir){
  message("link_dam2_metadata is deprecated, please use link_dam_metadata instead")
  link_dam_metadata(x, result_dir)
}


