context("Infotheo meta-features")

test_that("infotheo.result", {

  aux = infotheo(Species ~ ., iris)
  expect_named(aux, ls.infotheo())

  expect_equal(aux, infotheo(iris[1:4], iris[5]))
  expect_named(infotheo(Species ~ ., iris, ls.infotheo()[1:3]), 
               ls.infotheo()[1:3])
})

test_that("transform.false.result", {

  data = iris
  data[,1] = infotheo::discretize(data[,1])
  data[,2] = infotheo::discretize(data[,2])
  data[,3] = infotheo::discretize(data[,3])
  data[,4] = infotheo::discretize(data[,4])

  aux = infotheo(Species ~ ., data, transform=FALSE)
  expect_named(aux, ls.infotheo())

  expect_equal(aux, infotheo(data[1:4], data[5], transform=FALSE))
  expect_named(infotheo(Species ~ ., data, ls.infotheo()[1:3]), 
               ls.infotheo()[1:3])
})

test_that("infotheo.errors",{
  expect_error(infotheo(iris[1:130, 1:4], iris[5]))
  expect_error(infotheo(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(infotheo(Species ~ ., iris, features=c("abc", "xdef")))
})
