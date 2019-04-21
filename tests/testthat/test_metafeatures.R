context("Meta-features")

test_that("multiclass.result", {

  set.seed(123)
  aux = metafeatures(Species ~ ., iris)

  set.seed(123)
  expect_equal(aux, metafeatures(iris[1:4], iris[5]))
})

test_that("binary.result", {

  iris = iris[1:100,]
  iris$Species = factor(iris$Species)

  set.seed(123)
  aux = metafeatures(Species ~ ., iris)

  set.seed(123)
  expect_equal(aux, metafeatures(iris[1:4], iris[5]))
})

test_that("groups.result", {

  set.seed(123)
  aux = metafeatures(Species ~ ., iris, 
    c("general", "statistical", "infotheo", "model.based", "landmarking"))

  set.seed(123)
  gvals = unlist(general(Species ~ ., iris))
  svals = unlist(statistical(Species ~ ., iris))
  ivals = unlist(infotheo(Species ~ ., iris))
  mvals = unlist(model.based(Species ~ ., iris))
  lvals = unlist(landmarking(Species ~ ., iris))

  expect_equal(as.numeric(aux), 
    as.numeric(c(gvals, svals, ivals, mvals, lvals)))
})

test_that("validation.error",{
  expect_error(metafeatures(iris[1:130, 1:4], iris[5]))
  expect_error(metafeatures(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(metafeatures(Species ~ ., iris, groups=c("abc", "xdef")))
  expect_error(metafeatures(Species ~ ., runif(100)))
})
