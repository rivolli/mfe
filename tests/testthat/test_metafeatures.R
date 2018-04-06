context("Function meta-features")

test_that("metafeatures.result", {
  set.seed(123)
  aux1 <- metafeatures(Species ~ ., iris)
  
  set.seed(123)
  expect_equal(aux1, metafeatures(iris[1:4], iris[5]))
  
  aux2 <- metafeatures(Species ~ ., iris, c("general", "infotheo"))
  gvals <- unlist(general(Species ~ ., iris))
  ivals <- unlist(infotheo(Species ~ ., iris))
  names(gvals) <- paste("general", names(gvals), sep=".")
  names(ivals) <- paste("infotheo", names(ivals), sep=".")
  
  expect_equal(aux2, c(gvals, ivals))
})

test_that("landmarking.errors",{
  #Test errors cases
  expect_error(metafeatures(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(metafeatures(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(metafeatures(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(metafeatures(Species ~ ., iris, c("abc", "xdef")))
})
