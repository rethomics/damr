#' @import behavr
#' @import data.table
NULL

DAM2_COLS <-  as.list(c("i", "c", "c","i", rep("_",6), rep("i",32)))
names(DAM2_COLS) <- c("idx", "date", "time","status", sprintf("no_data_%02d", 1:6), sprintf("channel_%02d", 1:32))
