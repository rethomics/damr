dir_exists <- function(d) {
  de <- file.info(d)$isdir
  ifelse(is.na(de), FALSE, de)
}


check_dir_exists <- function(dir){
  if(!dir_exists(dir))
    stop(sprintf("The directory %s does not exist",dir))
}
