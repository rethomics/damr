#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="
#' @importFrom data.table "key"
#' @import behavr
NULL

DAM2_COLS <-  as.list(c("i", "c", "c","i", rep("_",6), rep("i",32)))
names(DAM2_COLS) <- c("idx", "date", "time","status", sprintf("no_data_%02d", 1:6), sprintf("channel_%02d", 1:32))
