context("read_dam2")

test_that("results match text file", {
  FILE <- damr_example("M064.txt")
  dt <- damr:::read_dam2_file(FILE, start_datetime = "2017-06-30 14:45:00", stop_datetime = +Inf)
  expect_equal(dt[t==10*60 & behavr::xmv(region_id)==15, activity], 10)
  expect_equal(dt[t==11*60 & behavr::xmv(region_id)==14, activity], 9)
})


