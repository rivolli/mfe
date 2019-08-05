context("Relative Landmarking meta-features")

test_that("landmarking.result", {

  set.seed(123)
  aux = relative(Species ~ ., iris)
  expect_named(aux, ls.relative())

  set.seed(123)
  expect_equal(aux, relative(iris[1:4], iris[5]))
  expect_named(relative(Species ~ ., iris, ls.relative()[1:3]), 
               ls.relative()[1:3])
})

test_that("score.result", {

  set.seed(123)
  aux = relative(Species ~ ., iris, size=0.6)
  set.seed(123)
  expect_equal(aux, relative(iris[1:4], iris[5], size=0.6))

  set.seed(123)
  aux = relative(Species ~ ., iris, size=0.6, score="balanced.accuracy")
  set.seed(123)
  expect_equal(aux, relative(iris[1:4], iris[5], size=0.6, 
    score="balanced.accuracy"))

  set.seed(123)
  aux = relative(Species ~ ., iris, size=0.6, score="kappa")
  set.seed(123)
  expect_equal(aux, relative(iris[1:4], iris[5], size=0.6, score="kappa"))
})

test_that("relative.errors",{
  expect_error(relative(iris[1:130, 1:4], iris[5]))
  expect_error(relative(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(relative(Species ~ ., iris, features=c("abc", "xdef")))
  expect_error(relative(Species ~ ., iris, size=0))
})
