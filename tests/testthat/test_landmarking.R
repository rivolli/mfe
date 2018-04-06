context("Landmarking meta-features")

test_that("landmarking.result", {
  set.seed(123)
  aux1 <- landmarking(Species ~ ., iris)
  
  expect_named(aux1, ls.landmarking())
  set.seed(123)
  expect_equal(aux1, landmarking(iris[1:4], iris[5]))
  expect_named(landmarking(Species ~ ., iris, ls.landmarking()[1:3]), 
               ls.landmarking()[1:3])
})

test_that("landmarking.errors",{
  #Test errors cases
  expect_error(landmarking(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(landmarking(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(landmarking(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(landmarking(Species ~ ., iris, features=c("abc", "xdef")))
})
