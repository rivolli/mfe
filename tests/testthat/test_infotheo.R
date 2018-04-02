context("Infotheo meta-features")

test_that("mf.infotheo.result", {
  aux1 <- mf.infotheo(Species ~ ., iris)
  
  expect_named(aux1, ls.infotheo())
  expect_equal(aux1, mf.infotheo(iris[1:4], iris[5]))
  expect_named(mf.infotheo(Species ~ ., iris, ls.infotheo()[1:3]), 
               ls.infotheo()[1:3])
})

test_that("mf.infotheo.errors",{
  #Test errors cases
  expect_error(mf.infotheo(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(mf.infotheo(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(mf.infotheo(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(mf.infotheo(Species ~ ., iris, features=c("abc", "xdef")))
})
