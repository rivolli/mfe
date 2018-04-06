context("Model based meta-features")

test_that("modelbased.result", {
  aux1 <- model.based(Species ~ ., iris)
  
  expect_named(aux1, ls.model.based())
  expect_equal(aux1, model.based(iris[1:4], iris[5]))
  expect_named(model.based(Species ~ ., iris, ls.model.based()[1:3]), 
               ls.model.based()[1:3])
})

test_that("model.based.errors",{
  #Test errors cases
  expect_error(model.based(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(model.based(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(model.based(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(model.based(Species ~ ., iris, features=c("abc", "xdef")))
})
