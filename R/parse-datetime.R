parse_datetime <- function(x, tz="UTC"){
  stopifnot(length(x)==1) # scalar only
  if(is.factor(x))
    x <-  as.character(x)

  if(is.infinite(x) | "POSIXct" %in% class(x))
    return(x)
  if(is.character(x)){
    out <- readr::parse_datetime(x, locale = readr::locale(tz = tz))
    readr::stop_for_problems(out)
    return(out)
  }
  stop("Unexpected type for x")
}
