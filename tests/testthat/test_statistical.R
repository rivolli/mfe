context("Statistical meta-features")

test_that("mf.statistical.result", {
  aux1 <- mf.statistical(Species ~ ., iris)
  
  expect_named(aux1, ls.statistical())
  expect_equal(aux1, mf.statistical(iris[1:4], iris[5]))
  expect_named(mf.statistical(Species ~ ., iris, ls.statistical()[1:3]), 
               ls.statistical()[1:3])
})

test_that("mf.statistical.transform", {
  aux <- cbind(class=iris$Species, iris)
  expect_equal(
    mf.statistical(Species ~ ., aux, ls.statistical()[1:5], transform=FALSE),
    mf.statistical(Species ~ ., iris, ls.statistical()[1:5], transform=FALSE)
  )
  
  expect_false(isTRUE(all.equal(
    mf.statistical(Species ~ ., aux, "sp", transform=FALSE),
    mf.statistical(Species ~ ., iris, "sp", transform=FALSE)
  )))
})

test_that("mf.statistical.errors",{
  #Test errors cases
  expect_error(mf.statistical(iris[1:130, 1:4], iris[5]),
               "x and y must have same number of rows")
  expect_error(mf.statistical(iris[, 1:4], iris[,3]),
               "y must contain classes values")
  expect_error(mf.statistical(as.matrix(iris[, c(1,2)]), iris$Species),
               "data argument must be a data.frame")
  expect_error(mf.statistical(Species ~ ., iris, features=c("abc", "xdef")))
})
