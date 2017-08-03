context("query_dam2")

test_that("query_dam2 works as expected on single files", {
  root_dir = damr_example_dir()
  query = data.table::data.table(file="M064.txt",
                      # note the time (10:00) is added as reference time
                      start_datetime = c("2017-07-01 10:00:00", "2017-07-02 10:00:00"),
                      stop_datetime = "2017-07-07",
                      region_id=c(1:32),
                      condition=rep(letters[1:2],each=16),
                      genotype=c("A", "A", "B", "B"))

  dt <- query_dam2(root_dir, query)
  # we have the data from query at the right place
  expect_identical(dt[meta=TRUE][,genotype,keyby=region_id], query[,genotype,keyby=region_id])

  # implicit region 1:32
  query = data.table::data.table(file="M064.txt",
                                 # note the time (10:00) is added as reference time
                                 start_datetime = c("2017-07-01 10:00:00"),
                                 stop_datetime = "2017-07-07")

  dt <- query_dam2(root_dir, query)
  expect_equal(nrow(unique(dt[,"id"])), 32)
  expect_identical(dt[meta=TRUE]$region_id, 1:32)
})




test_that("query_dam2 works as expected on two files", {
  sample_files <- c("M064.txt", "M014.txt")
  root_dir = damr_example_dir()
  query = data.table::data.table(file=rep(sample_files, each=32),
                                 # note the time (10:00) is added as reference time
                                 start_datetime = c("2017-07-01 10:00:00"),
                                 stop_datetime = "2017-07-07",
                                 region_id=c(1:32),
                                 condition=rep(letters[1:2],each=16),
                                 genotype=c("A", "A", "B", "B"))

  dt <- query_dam2(root_dir, query)

  # we have the data from query at the right place
  expect_identical(dt[meta=TRUE][,genotype,keyby=region_id], query[,genotype,keyby=region_id])

})



test_that("query_dam2 fails if file does not exist", {
  sample_files <- "NotAFile.txt"
  root_dir = damr_example_dir()
  query = data.table::data.table(file=rep(sample_files, each=32),
                                 # note the time (10:00) is added as reference time
                                 start_datetime = c("2017-07-01 10:00:00"),
                                 stop_datetime = "2017-07-07",
                                 region_id=c(1:32),
                                 condition=rep(letters[1:2],each=16),
                                 genotype=c("A", "A", "B", "B"))

  expect_error(query_dam2(root_dir, query), regex="does not exist")
})


test_that("query_dam2 fails if columns are not define in query", {
  sample_files <- "M064.txt"
  root_dir = damr_example_dir()
  query = data.table::data.table(filea=rep(sample_files, each=32),
                                 # note the time (10:00) is added as reference time
                                 start_datetime = c("2017-07-01 10:00:00"),
                                 stop_datetime = "2017-07-07",
                                 region_id=c(1:32),
                                 condition=rep(letters[1:2],each=16),
                                 genotype=c("A", "A", "B", "B"))

  expect_error(query_dam2(root_dir, query), regex="MUST have.*three columns")
})


#
# single_file_query <- data.frame(file="M064.txt",
#                                 # note the time (10:00) is added as reference time
#                                 start_datetime = c("2017-07-01 10:00:00", "2017-07-02 10:00:00"),
#                                 stop_datetime = "2017-07-07",
#                                 region_id=c(1:32),
#                                 condition=rep(letters[1:2],each=16),
#                                 genotype=c("A", "A", "B", "B"))
#
# sample_files <- c("M064.txt", "M014.txt")
# two_files_query <- data.frame(file=rep(sample_files, each=32),
#                                  # note the time (10:00) is added as reference time
#                                  start_datetime = c("2017-07-01 10:00:00"),
#                                  stop_datetime = "2017-07-07",
#                                  region_id=c(1:32),
#                                  condition=rep(letters[1:2],each=16),
#                                  genotype=c("A", "A", "B", "B"))
#
# library(devtools)
# devtools::use_data(single_file_query, overwrite = T)
# devtools::use_data(two_files_query, overwrite = T)

