context("Infotheo meta-features")

test_that("infotheo.result", {
  aux1 <- infotheo(Species ~ ., iris)
  
  expect_named(aux1, ls.infotheo())
  expect_equal(aux1, infotheo(iris[1:4], iris[5]))
  expect_named(infotheo(Species ~ ., iris, ls.infotheo()[1:3]), 
               ls.infotheo()[1:3])
})

test_that("infotheo.errors",{
  #Test errors cases
  expect_error(infotheo(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(infotheo(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(infotheo(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(infotheo(Species ~ ., iris, features=c("abc", "xdef")))
})
