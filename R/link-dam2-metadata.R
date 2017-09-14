

#' Link DAM2 metadata to result files
#'
#' This function checks and add columns to DAM2 metadata.
#' The new columns are necessary to locate the result files, and allocate individual unique identifiers.
#'
#' @param x object such as a [data.frame], or the name of a file (see detail)
#' @param result_dir the root directory where all daily data are saved
#' @return a [data.table::data.table] with the same rows as x, and extra columns for further data loading
#' @details
#' These function will link augment metadata from two different types of inputs:
#' 1. A [data.frame] (recomended)
#' In this case, the function will try to match requested data with data available on `result_dir`.
#' The provided [data.table] has typically one row per requested individial and the columns
#' (not necessarily in this order):
#'     * `machine_name` -- the name of the machine in which the individual was (e.g. `"ETHOSCOPE_001"`)
#'     * `date` -- the start date of the experiment formated as `"YYYY-MM-DD"`
#'     * `region_id` -- the ROI in which the animal was. When *not provided, all regions are queried*.
#'     * `time` -- the start time of the experiment formated as "HH:MM:SS".
#'        When *not provided*, and multiple expriment for the same machine exist, *only the last one is loaded*.
#'     * `???` -- any number of arbitrary columns* to associate `conditions`/`treatments`/`genotypes`/... to the previous columns.
#' 2. The name of a CSV file that contains a table as described in `1`.
#'
#' Each row of the metadata describes one individual with one set of conditions (when `region_id` is specified),
#' or in each monitor (when it is not).
#' It must have  the following columns:
#' * `file` -- the location of a data file (e.g. `"Monitor3.txt"`).
#' * `start_datetime` -- the first day **and time** of the requested experiment (e.g. `"2014-12-28 18:00:00"`).
#' * `stop_datetime` -- the last day and time of the requested experiment (e.g. `"2014-12-30  19:00:00"` or simply `"2014-12-30"`).
#' * `region_id` -- the channel (between 1 and 32) in which the animal was in (e.g. `20`).
#'   `region_id` is optional. If not provided, all 32 channels are loaded *with the same conditions*.
#' * `???` *any number of arbitrary columns* to associate `conditions`/`treatments`/`genotypes`/... to the previous columns.
#'
#' The time in data is expressed relatively to start_date.
#' In other words, if you do circadian analysis, and your `D->L`` transitions are at 09:00:00, you want to set
#' `start_datetime = "YYY-MM-DD 09:00:00"`. The root directory is the folder where your files live.
#' For instance, `result_dir = "C:/where/I/Store/my/txt/files/"`
#' @seealso
#' * [load_dam2] -- to subsequently load the actual data
#' @references
#' * [metadata tutorial](https://rethomics.github.io/metadata.html) -- how to work with metadata
#' @export
link_dam2_metadata <- function(x, result_dir){

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

  q[,id:=sprintf("%02d|%s",
                 region_id,
                 experiment_id)]
  q[, experiment_id := NULL]

  data.table::setkeyv(q, "id")

  file_info <- q[,.(file_info =  list(list(path = path, file = basename(path)))), by="id"]
  out <- file_info[q]
  out[, path := NULL]
  out


}
