context("Concept meta-features")

test_that("concept.result", {
  aux = concept(Species ~ ., iris)
  
  expect_named(aux, ls.concept())
  expect_equal(aux, concept(iris[1:4], iris[5]))
  expect_named(concept(Species ~ ., iris, ls.concept()[1:3]), 
               ls.concept()[1:3])
})

test_that("clustering.errors",{
  expect_error(concept(iris[1:130, 1:4], iris[5]))
  expect_error(concept(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(concept(Species ~ ., iris, features=c("abc", "xdef")))
})
