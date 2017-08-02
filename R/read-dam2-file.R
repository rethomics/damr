#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="

DAM2_COLS <-  as.list(c("i", "c", "c","i", rep("_",6), rep("i",32)))
names(DAM2_COLS) <- c("idx", "date", "time","status", sprintf("no_data_%02d", 1:6), sprintf("channel_%02d", 1:32))


read_dam2_file <- function(file,
                            start_datetime=-Inf,
                            stop_datetime=+Inf,
                            tz="UTC"){
  start_datetime <- parse_datetime(start_datetime,tz=tz)
  stop_datetime <- parse_datetime(stop_datetime,tz=tz)

  first_last_lines <- find_dam2_first_last_lines(file,
                                                 start_datetime,
                                                 stop_datetime,
                                                 tz)
  first_line = first_last_lines$id[1]
  last_line = first_last_lines$id[2]
  col_types=do.call(readr::cols_only, DAM2_COLS)
  df <-readr::read_tsv(file, col_names = names(DAM2_COLS),
                                 col_types = col_types,
                                 skip = first_line-1,
                                 n_max = last_line-first_line+1,
                                 progress = F)

#  return(start_datetime)
  df <- df %>%
        dplyr::mutate(
          datetime = paste(date,time, sep=" "),
          datetime = as.POSIXct(strptime(datetime,"%d %b %y %H:%M:%S",tz=tz))
        )

   # if start date is not define, t0 is the first read available, whether or not is is valid!
  if(is.infinite(start_datetime))
    t0 = df$datetime[1]
  else
    t0 = start_datetime

  experiment_id <- paste(start_datetime, basename(file),sep="|")
  df <- df %>%
        dplyr::filter(status ==1) %>% # valid reads
        dplyr::distinct(datetime, .keep_all = TRUE) %>% # remove possible duplicates
        dplyr::select(dplyr::matches("(channel)|(datetime)")) %>%
        dplyr::select("0"=dplyr::starts_with("channel_"), dplyr::everything()) %>%
        tidyr::gather(channel,activity,-datetime) %>%
        dplyr::transmute(id= sprintf("%02d|%s",as.integer(channel),experiment_id),
                         region_id = as.integer(channel),
                         t=hms::as.hms(datetime-t0),
                         activity=activity
                         )
  dt <- data.table::data.table(df,key="id")
  meta <- unique(dt[, c("id","region_id")],by="id")

  meta[,experiment_id := experiment_id]
  meta[,start_datetime:=start_datetime]
  meta[,file:=basename(file)]
  meta[,file:=basename(file)]

  dt[,region_id:=NULL]
  behavr::behavr(dt,meta)
}
