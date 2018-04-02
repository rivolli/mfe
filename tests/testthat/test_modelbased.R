context("Model based meta-features")

test_that("mf.modelbased.result", {
  aux1 <- mf.model.based(Species ~ ., iris)
  
  expect_named(aux1, ls.model.based())
  expect_equal(aux1, mf.model.based(iris[1:4], iris[5]))
  expect_named(mf.model.based(Species ~ ., iris, ls.model.based()[1:3]), 
               ls.model.based()[1:3])
})

test_that("mf.model.based.errors",{
  #Test errors cases
  expect_error(mf.model.based(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(mf.model.based(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(mf.model.based(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(mf.model.based(Species ~ ., iris, features=c("abc", "xdef")))
})
