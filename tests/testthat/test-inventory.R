test_that("Downloading works and returns 5 column data frame for stationType = 'plu'", {
  expect_equal(ncol(inventory('ACRE', stationType = 'plu')), 6)
})

test_that("Downloading works and returns 6 column data frame for stationType = 'flu'", {
  expect_equal(ncol(inventory('ACRE', stationType = 'flu')), 7)
})
