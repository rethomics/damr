find_dam2_first_last_lines <- function(file,
                                       start_datetime=-Inf,
                                       stop_datetime=+Inf,
                                       tz="UTC"){
  start_datetime <- parse_datetime(start_datetime,tz=tz)
  stop_datetime <- parse_datetime(stop_datetime,tz=tz)

  if(start_datetime > stop_datetime)
    stop("start_datetime is greater than stop_datetime. Cannot fetch any data!")

  datetimes_df <-readr::read_tsv(file, col_names = names(DAM2_COLS),
                                 col_types = readr::cols_only(date="c", time="c",status="i"),
                                 progress = F)

  datetimes_df <-
    datetimes_df %>%
    dplyr::mutate(id=1:n()) %>%
    dplyr::filter(status==1) %>%
    dplyr::transmute(
      id=id,
      datetime = paste(date,time, sep=" "),
      datetime = as.POSIXct(strptime(datetime,"%d %b %y %H:%M:%S",tz=tz)),
      diff_t = as.numeric(datetime-dplyr::lag(datetime), unit="secs")
    )  %>%
    dplyr::filter(datetime >= start_datetime & datetime <= stop_datetime)

  ## duplicated time stamps, clock changes?
  n_dups <- sum(duplicated(datetimes_df$datetime))
  if(n_dups > 50){
    stop("More than 50 duplicated dates entries in the queries file.
         This is a likely instance of the recording computer changing time
         (e.g. between winter and summer time)")
  }

  if(n_dups > 0){
    warning(sprintf("Some of the dates are repeated between successive measument in %s.",
                    file))
  }

  sampling_periods <- unique(na.omit(datetimes_df$diff_t))
  if(any(abs(sampling_periods) >= 3600))
    stop("Time has jumped for an hour or more!
          No valid data duting this time.
          Possibly device got disconected, of change to summer/winter time")

  ## irregular time stamps, possible missing reads
  n_sampling_periods <- length(sampling_periods)

  if(n_sampling_periods > 1)
    warning(sprintf("The sampling period is not always regular in %s.
                    Some reads must have been skipped.",file))

  first_and_last <- datetimes_df %>%
    dplyr::filter(row_number()==1 | row_number()==n())

  if(nrow(first_and_last) !=2)
    stop("No data in selected date range")
  first_and_last
}
