find_dam_first_last_lines <- function(file,
                                       start_datetime = -Inf,
                                       stop_datetime = +Inf,
                                       tz = "UTC"){
  . = data_type = status = datetime =  time =  datetime_posix = diff_t = NULL
  id = read_id = NULL


  start_datetime <- parse_datetime(start_datetime,tz=tz)

  has_time <- TRUE

  if(is.character(stop_datetime)){
    if(any(grep("^\\s*[0-9]{4}-[0-9]{2}-[0-9]{2}\\s*$", stop_datetime)))
      has_time <- FALSE
  }


  stop_datetime <- parse_datetime(stop_datetime,tz=tz)

  # we stop_date should be exclusive
  if(!has_time)
    stop_datetime <- stop_datetime + days(1) - .01

  if(start_datetime > stop_datetime)
    stop("start_datetime is greater than stop_datetime. Cannot fetch any data!")

  if(tools::file_ext(file) == "zip")
    fun <- fread_zip
  else
    fun <- fread

  cols_to_keep <- which(names(DAM5_COLS) %in% c("date", "time", "status", "data_type"))
  datetimes_dt <- fun(file,
                        col.names = names(DAM5_COLS)[cols_to_keep],
                        select =cols_to_keep)

  datetimes_dt[, id := 1:.N]
  datetimes_dt[, read_id := cumsum(data_type == "CT" | data_type == "0")]
  datetimes_dt <- datetimes_dt[status == 1]
  datetimes_dt <- datetimes_dt[, datetime := paste(date,time, sep=" ")]
  suppressWarnings(
    datetimes_dt <- datetimes_dt[, datetime_posix  := as.POSIXct(strptime(datetime,"%d %b %y %H:%M:%S",tz=tz))]
  )
  datetimes_dt <- datetimes_dt[datetime_posix >= start_datetime & datetime_posix <= stop_datetime]
  datetimes_dt[, diff_t := as.numeric(datetime_posix - shift(datetime_posix), unit="secs")]


  ## duplicated time stamps, clock changes?
  n_dups <- sum(duplicated(unique(datetimes_dt, by = "read_id")$datetime_posix))
  if(n_dups > 50){
    stop("More than 50 duplicated dates entries in the metadata file.
         This is a likely instance of the recording computer changing time
         (e.g. between winter and summer time)")
  }

  if(n_dups > 0){
    warning(sprintf("Some of the dates are repeated between successive measuments in %s.",
                    file))
  }

  sampling_periods <- unique(na.omit(unique(datetimes_dt, by = "read_id")$diff_t))
  if(any(abs(sampling_periods) >= 3600))
    stop("Time has jumped for an hour or more!
          No valid data during this time.
          Possibly, device was disconected or maybe a change from summer to winter time")

  ## irregular time stamps, possible missing reads
  n_sampling_periods <- length(sampling_periods)

  if(n_sampling_periods > 1)
    warning(sprintf("The sampling period is not always regular in %s.
                    Some reads must have been skipped.",file))

  first_and_last <- datetimes_dt[c(1, .N)]

  if(nrow(first_and_last) !=2)
    stop("No data in selected date range")
  first_and_last[, datetime := NULL]
  setnames(first_and_last, "datetime_posix", "datetime")
  first_and_last
}

# from https://stackoverflow.com/questions/8986818/automate-zip-file-reading-in-r
fread_zip <- function(zipfile, ...) {
  # Function reads data from a zipped csv file
  # Uses fread from the data.table package

  ## Create the temporary directory or flush CSVs if it exists already
  if (!file.exists(tempdir())) {dir.create(tempdir())
  } else {file.remove(list.files(tempdir(), full.names = TRUE, pattern = "*.csv|*.txt"))
  }

  ## Unzip the file into the dir
  unzip(zipfile, exdir=tempdir())

  ## Get path to file
  file <- list.files(tempdir(), pattern = "*.csv|*.txt", full.names = T)
    ## Throw an error if there's more than one
  if(length(file)>1) stop(paste("More than one data file inside zip:\n",file , collapse = ", "))

    ## Read the file
    out <- fread(file,
          #na.strings = c(""), # read empty strings as NA
          ...
    )
  out
}
