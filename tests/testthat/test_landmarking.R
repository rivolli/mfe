context("Landmarking meta-features")

test_that("mf.landmarking.result", {
  set.seed(123)
  aux1 <- mf.landmarking(Species ~ ., iris)
  
  expect_named(aux1, ls.landmarking())
  set.seed(123)
  expect_equal(aux1, mf.landmarking(iris[1:4], iris[5]))
  expect_named(mf.landmarking(Species ~ ., iris, ls.landmarking()[1:3]), 
               ls.landmarking()[1:3])
})

test_that("mf.landmarking.errors",{
  #Test errors cases
  expect_error(mf.landmarking(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(mf.landmarking(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(mf.landmarking(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(mf.landmarking(Species ~ ., iris, features=c("abc", "xdef")))
})
