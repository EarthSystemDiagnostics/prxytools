test_that("Bin averaging by indices works", {

  x <- 1 : 10
  
  expect_error(AverageByIndex(x, ind = c(-1, 5, 8)),
               "Indices all must be >= 1.")
  expect_error(AverageByIndex(x, ind = c(0, 5, 8)),
               "Indices all must be >= 1.")

  expect_error(AverageByIndex(x, ind = c(5, 3, 8)),
               "Indices must be monotonically increasing.")

  expect_error(AverageByIndex(x, ind = c(1, 5, 13)),
               "Index out of data index range.")

  expect_equal(AverageByIndex(x, ind = c(1, 4, 7, 9)), c(2, 5, 7.5))
  expect_equal(AverageByIndex(x, ind = c(1, 4, 7, 10)), c(2, 5, 8.5))
  expect_equal(AverageByIndex(x, ind = c(4, 7)), 5)

  x <- c(NA, x[-1])
  expect_equal(AverageByIndex(x, ind = c(1, 4, 7, 9)), c(2.5, 5, 7.5))
  expect_equal(AverageByIndex(x, ind = c(1, 4, 7, 9), na.rm = FALSE),
               c(NA, 5, 7.5))

})
