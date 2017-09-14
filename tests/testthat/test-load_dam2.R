context("load_dam2")

test_that("load_dam2 works as expected on two files", {
  sample_files <- c("M064.txt", "M014.txt")
  root_dir = damr_example_dir()
  metadata = data.table::data.table(file=rep(sample_files, each=32),
                                    # note the time (10:00) is added as reference time
                                    start_datetime = c("2017-07-01 10:00:00"),
                                    stop_datetime = "2017-07-07",
                                    region_id=c(1:32),
                                    condition=rep(letters[1:2],each=16),
                                    genotype=c("A", "A", "B", "B"))

  q <- link_dam2_metadata(metadata,root_dir)
  dt <- load_dam2(q)

  # we have the data from query at the right place
  expect_identical(dt[meta=TRUE][,genotype,keyby=region_id], metadata[,genotype,keyby=region_id])

})





test_that("query_dam2 fails if file does not exist", {
  sample_files <- "NotAFile.txt"
  root_dir = damr_example_dir()
  metadata = data.table::data.table(file=rep(sample_files, each=32),
                                 # note the time (10:00) is added as reference time
                                 start_datetime = c("2017-07-01 10:00:00"),
                                 stop_datetime = "2017-07-07",
                                 region_id=c(1:32),
                                 condition=rep(letters[1:2],each=16),
                                 genotype=c("A", "A", "B", "B"))

  q <- link_dam2_metadata(metadata,root_dir)
  expect_error(load_dam2(q), regex="does not exist")
})

