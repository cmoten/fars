test_that("file name created correctly", {
  test_string <- make_filename(2013)
  expect_match(test_string,'accident_2013.csv.bz2')
})
