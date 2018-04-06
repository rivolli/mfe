context("General meta-features")

test_that("general.result", {
  aux1 <- general(Species ~ ., iris)

  expect_named(aux1, ls.general())
  expect_equal(aux1, general(iris[1:4], iris[5]))
  expect_named(general(Species ~ ., iris, ls.general()[1:3]), 
               ls.general()[1:3])
})

test_that("general.errors",{
  #Test errors cases
  expect_error(general(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(general(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(general(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(general(Species ~ ., iris, features=c("abc", "xdef")))
})
