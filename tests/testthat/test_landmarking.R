context("Landmarking meta-features")

test_that("landmarking.result", {

  set.seed(123)
  aux = landmarking(Species ~ ., iris)
  expect_named(aux, ls.landmarking())

  set.seed(123)
  expect_equal(aux, landmarking(iris[1:4], iris[5]))
  expect_named(landmarking(Species ~ ., iris, ls.landmarking()[1:3]), 
               ls.landmarking()[1:3])
})

test_that("score.result", {

  set.seed(123)
  aux = landmarking(Species ~ ., iris, score="accuracy")
  set.seed(123)
  expect_equal(aux, landmarking(iris[1:4], iris[5], score="accuracy"))

  set.seed(123)
  aux = landmarking(Species ~ ., iris, score="balanced.accuracy")
  set.seed(123)
  expect_equal(aux, landmarking(iris[1:4], iris[5], score="balanced.accuracy"))

  set.seed(123)
  aux = landmarking(Species ~ ., iris, score="kappa")
  set.seed(123)
  expect_equal(aux, landmarking(iris[1:4], iris[5], score="kappa"))

  set.seed(123)
  aux = landmarking(Species ~ ., iris, size=0.5)
  set.seed(123)
  expect_equal(aux, landmarking(iris[1:4], iris[5], size=0.5))
})

test_that("landmarking.errors",{
  expect_error(landmarking(iris[1:130, 1:4], iris[5]))
  expect_error(landmarking(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(landmarking(Species ~ ., iris, features=c("abc", "xdef")))
  expect_error(landmarking(Species ~ ., iris, size=0))
})
