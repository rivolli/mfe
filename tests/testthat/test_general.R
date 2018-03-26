context("General meta-features")

test_that("mf.general.result", {
  aux1 <- mf.general(Species ~ ., iris)

  expect_named(aux1, ls.general())
  expect_equal(aux1, mf.general(iris[1:4], iris[5]))
  expect_named(mf.general(Species ~ ., iris, ls.general()[1:3]), 
               ls.general()[1:3])
})

test_that("mf.general.errors",{
  #Test errors cases
  expect_error(mf.general(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(mf.general(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(mf.general(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(mf.general(Species ~ ., iris, features=c("abc", "xdef")))
})
