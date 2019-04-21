context("Statistical meta-features")

test_that("statistical.result", {
  aux = statistical(Species ~ ., iris)
  expect_named(aux, ls.statistical())

  expect_equal(aux, statistical(iris[1:4], iris[5]))
  expect_named(statistical(Species ~ ., iris, ls.statistical()[1:3]), 
               ls.statistical()[1:3])
})

test_that("byclass.result", {
  aux = statistical(Species ~ ., iris, by.class=TRUE)
  expect_named(aux, ls.statistical())

  expect_equal(aux, statistical(iris[1:4], iris[5], by.class=TRUE))
  expect_named(statistical(Species ~ ., iris, ls.statistical()[1:3]), 
               ls.statistical()[1:3])
})

test_that("statistical.transform", {
  aux <- cbind(class=iris$Species, iris)
  expect_equal(
    statistical(Species ~ ., aux, ls.statistical()[1:5], transform=FALSE),
    statistical(Species ~ ., iris, ls.statistical()[1:5], transform=FALSE)
  )
  
  expect_false(isTRUE(all.equal(
    statistical(Species ~ ., aux, "sp", transform=FALSE),
    statistical(Species ~ ., iris, "sp", transform=FALSE)
  )))
})

test_that("statistical.errors",{
  expect_error(statistical(iris[1:130, 1:4], iris[5]))
  expect_error(statistical(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(statistical(Species ~ ., iris, features=c("abc", "xdef")))
})
