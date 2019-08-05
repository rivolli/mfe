context("Clustering meta-features")

test_that("clustering.result", {
  aux = clustering(Species ~ ., iris)
  
  expect_named(aux, ls.clustering())
  expect_equal(aux, clustering(iris[1:4], iris[5]))
  expect_named(clustering(Species ~ ., iris, ls.clustering()[1:3]), 
               ls.clustering()[1:3])
})

test_that("clustering.errors",{
  expect_error(clustering(iris[1:130, 1:4], iris[5]))
  expect_error(clustering(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(clustering(Species ~ ., iris, features=c("abc", "xdef")))
})
