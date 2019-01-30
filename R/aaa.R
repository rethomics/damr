#' @import behavr
#' @import data.table
#' @importFrom stats "na.omit"
#' @importFrom utils "unzip"
. = NULL
NULL

DAM2_COLS <-  as.list(c("i", "c", "c","i", rep("_",6), rep("i",32)))
names(DAM2_COLS) <- c("idx", "date", "time","status", sprintf("no_data_%02d", 1:6), sprintf("channel_%02d", 1:32))

DAM5_COLS <-  DAM2_COLS
DAM5_COLS[[8]] <- "c"
names(DAM5_COLS)[[8]] <- "data_type"


DATA_TYPE_NAMES <- c("C1"= "C1",
       "C2" = "C2",
       "C3" = "C3",
       "C4" = "C4",
       "CT" = "activity",
       "D1" = "D1",
       "D2" = "D2",
       "D3" = "D3",
       "D4" = "D4",
       "Pn" = "Pn",
       "0" = "activity",
       "PnF" = "Pn",
       "Ct" = "activity")
