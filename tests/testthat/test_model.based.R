context("Model based meta-features")

test_that("model.based.result", {
  aux = model.based(Species ~ ., iris)
  
  expect_named(aux, ls.model.based())
  expect_equal(aux, model.based(iris[1:4], iris[5]))
  expect_named(model.based(Species ~ ., iris, ls.model.based()[1:3]), 
               ls.model.based()[1:3])
})

test_that("model.based.errors",{
  expect_error(model.based(iris[1:130, 1:4], iris[5]))
  expect_error(model.based(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(model.based(Species ~ ., iris, features=c("abc", "xdef")))
})
