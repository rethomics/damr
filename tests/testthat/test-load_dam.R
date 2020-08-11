context("load_dam")

test_that("load_dam works as expected on two files", {
  sample_files <- c("M064.txt", "M014.txt")
  root_dir = damr_example_dir()
  metadata = data.table::data.table(file=rep(sample_files, each=32),
                                    # note the time (10:00) is added as reference time
                                    start_datetime = c("2017-07-01 10:00:00"),
                                    stop_datetime = "2017-07-07",
                                    region_id=c(1:32),
                                    condition=rep(letters[1:2],each=16),
                                    genotype=c("A", "A", "B", "B"))

  q <- link_dam_metadata(metadata,root_dir)
  dt <- load_dam(q)

  expect_identical(dt[meta=TRUE][,genotype,keyby=region_id], metadata[,genotype,keyby=region_id])

})



test_that("load_dam works with explicit time in stop_date", {
  sample_files <- c("M064.txt", "M014.txt")
  root_dir = damr_example_dir()
  metadata = data.table::data.table(file=rep(sample_files, each=32),
                                    # note the time (10:00) is added as reference time
                                    start_datetime = c("2017-07-01 10:00:00"),
                                    stop_datetime = "2017-07-07 23:00:00",
                                    region_id=c(1:32),
                                    condition=rep(letters[1:2],each=16),
                                    genotype=c("A", "A", "B", "B"))

  q <- link_dam_metadata(metadata,root_dir)
  dt <- load_dam(q)
  print(dt)
  expect_identical(dt[meta=TRUE][,genotype,keyby=region_id], metadata[,genotype,keyby=region_id])

})



test_that("load_dam works as expected with IDate and Date ", {
  sample_files <- c("M064.txt", "M014.txt")
  root_dir = damr_example_dir()
  metadata = data.table::data.table(file=rep(sample_files, each=32),
                                    # note the time (10:00) is added as reference time
                                    start_datetime = c("2017-07-01 10:00:00"),
                                    stop_datetime = "2017-07-07",
                                    region_id=c(1:32),
                                    condition=rep(letters[1:2],each=16),
                                    genotype=c("A", "A", "B", "B"))

  q <- link_dam_metadata(metadata,root_dir)

  dt1 <- load_dam(q)

  q[, stop_datetime := as.Date(stop_datetime)]
  dt2 <- load_dam(q)
  q[, stop_datetime := as.IDate(stop_datetime)]
  dt3 <- load_dam(q)

  expect_identical(dt1[,activity], dt2[,activity])
  expect_identical(dt3[,activity], dt2[,activity])

})

test_that("query_dam fails if file does not exist", {
  sample_files <- "NotAFile.txt"
  root_dir = damr_example_dir()
  metadata = data.table::data.table(file=rep(sample_files, each=32),
                                 # note the time (10:00) is added as reference time
                                 start_datetime = c("2017-07-01 10:00:00"),
                                 stop_datetime = "2017-07-07",
                                 region_id=c(1:32),
                                 condition=rep(letters[1:2],each=16),
                                 genotype=c("A", "A", "B", "B"))

  q <- link_dam_metadata(metadata,root_dir)
  expect_error(load_dam(q), regex="does not exist")
})

