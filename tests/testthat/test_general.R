context("General meta-features")

test_that("general.result", {
  aux = general(Species ~ ., iris)
  expect_named(aux, ls.general())

  expect_equal(aux, general(iris[1:4], iris[5]))
  expect_named(general(Species ~ ., iris, ls.general()[1:3]), ls.general()[1:3])
})

test_that("general.errors",{
  expect_error(general(iris[1:130, 1:4], iris[5]))
  expect_error(general(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(general(Species ~ ., iris, features=c("abc", "xdef")))
})
