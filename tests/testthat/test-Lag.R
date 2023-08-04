test_that("vector shifting works", {

  x <- 1 : 5

  expect_equal(Lag(x, shift = 0), x)
  expect_equal(Lag(x), c(NA, 1 : 4))
  expect_equal(Lag(x, shift = 5), as.integer(rep(NA, 5)))
  expect_equal(Lag(x, -2), c(3 : 5, NA, NA))

  x <- c("a", "b", "c")
  expect_equal(Lag(x, 2), c("", "", "a"))
  expect_equal(Lag(x, -3), rep("", 3))

  x <- factor(c("foo", "bar"), levels = c("foo", "bar", "boo"))
  expect_equal(Lag(x), factor(c(NA, "foo"), levels = c("foo", "bar", "boo")))

  x <- 1 : 5
  lab <- "Some label for x"
  s <- 3
  attr(x, which = "label") <- lab
  xs <- Lag(x, s)

  expect_equal(attr(xs, which = "label"),
               paste(lab, "lagged", s, "observations"))
})
