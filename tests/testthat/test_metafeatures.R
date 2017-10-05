context("Meta-features test")

test_that("metafeatures.result", {
  set.seed(1)
  features <- metafeatures(Species ~ ., iris)
  expect_is(features, "numeric")

  set.seed(1)
  expect_equal(features, metafeatures(Species ~ ., iris, ls.metafeatures()))
  set.seed(1)
  expect_equal(features, metafeatures(iris[1:4], iris$Species))

  #Test general group
  general <- metafeatures(Species ~ ., iris, "general")
  expected <- unlist(mf.general(Species ~ ., iris))
  names(expected) <- paste("general", names(expected), sep='.')
  expect_equal(general, expected)
  expect_equal(features[names(general)], general)

  #Test statistical group
  statistical <- metafeatures(Species ~ ., iris, "statistical")
  expected <- unlist(mf.statistical(Species ~ ., iris))
  names(expected) <- paste("statistical", names(expected), sep='.')
  expect_equal(statistical, expected)
  expect_equal(features[names(statistical)], statistical)

  #Test infotheo group
  infotheo <- metafeatures(Species ~ ., iris, "infotheo")
  expected <- unlist(mf.infotheo(Species ~ ., iris))
  names(expected) <- paste("infotheo", names(expected), sep='.')
  expect_equal(infotheo, expected)
  expect_equal(features[names(infotheo)], infotheo)

  #Test discriminant group
  discriminant <- metafeatures(Species ~ ., iris, "discriminant")
  expected <- unlist(mf.discriminant(Species ~ ., iris))
  names(expected) <- paste("discriminant", names(expected), sep='.')
  expect_equal(discriminant, expected)
  expect_equal(features[names(discriminant)], discriminant)

  #Test model.based group
  modelbased <- metafeatures(Species ~ ., iris, "model.based")
  expected <- unlist(mf.model.based(Species ~ ., iris))
  names(expected) <- paste("model.based", names(expected), sep='.')
  expect_equal(modelbased, expected)
  expect_equal(features[names(modelbased)], modelbased)

  #Test landmarking group
  set.seed(1)
  landmarking <- metafeatures(Species ~ ., iris, "landmarking")
  set.seed(1)
  expected <- unlist(mf.landmarking(Species ~ ., iris))
  names(expected) <- paste("landmarking", names(expected), sep='.')
  expect_equal(landmarking, expected)
  expect_equal(features[names(landmarking)], landmarking)

  expect_equal(features, c(discriminant, general, infotheo, landmarking, modelbased, statistical))
})

test_that("metafeatures.post.processing", {
  features1 <- metafeatures(Species ~ ., iris,
                            c("general", "statistical", "infotheo"),
                            c("max", "min"))
  features2 <- metafeatures(Species ~ ., iris,
                            c("general", "statistical", "infotheo"),
                            c("max", "mean", "min"))
  features3 <- metafeatures(Species ~ ., iris,
                            c("general", "statistical", "infotheo"),
                            c("max", "mean", "skewness", "min"))
  expect_lt(length(features1), length(features2))
  expect_lt(length(features2), length(features3))
  expect_equal(length(features2) - length(features1), length(features3) - length(features2))

  measures <- paste("statistical", ls.statistical(), sep='.')
  g1 <- paste(measures, rep(c("max", "min"), each=length(measures)), sep=".")
  g2 <- paste(measures, rep(c("max", "mean", "min"), each=length(measures)), sep=".")
  g3 <- paste(measures, rep(c("max", "mean", "skewness", "min"), each=length(measures)), sep=".")
  expect_false(any(is.null(features1[g1])))
  expect_false(any(is.null(features2[g2])))
  expect_false(any(is.null(features3[g3])))
})

test_that("metafeatures.errors",{
  #Test erros cases
  expect_error(metafeatures(iris[1:100, 1:4], iris[1,43, 5]),
               "x and y must have same number of rows")
  expect_error(metafeatures(as.matrix(iris[1:4]), iris[5]),
               "data argument must be a data.frame")
  expect_error(metafeatures.formula(iris[1:4], iris[5]),
               "method is only for formula datas")
  expect_error(metafeatures(Species ~ ., iris, groups=c("abc", "xdef")))
})
