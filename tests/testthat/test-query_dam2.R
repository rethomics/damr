context("query_dam2")

test_that("Start and stop dates work as expected", {
  sample_file <- damr_example("M064.txt")
  query = data.table::data.table(path=sample_file,
                      # note the time (10:00) is added as reference time
                      start_datetime = c("2017-07-01 10:00:00", "2017-07-02 10:00:00"),
                      stop_datetime = "2017-07-07",
                      region_id=c(1:32),
                      condition=rep(letters[1:2],each=16),
                      genotype=c("A", "A", "B", "B"))

  dt <- query_dam2(query)
  # we have the data from query at the right place
  expect_identical(dt[meta=TRUE][,genotype,keyby=region_id], query[,genotype,keyby=region_id])

  # implicit region 1:32
  query = data.table::data.table(path=sample_file,
                                 # note the time (10:00) is added as reference time
                                 start_datetime = c("2017-07-01 10:00:00"),
                                 stop_datetime = "2017-07-07")

  dt <- query_dam2(query)
  expect_equal(nrow(unique(dt[,"id"])), 32)
  expect_identical(dt[meta=TRUE]$region_id, 1:32)
})


