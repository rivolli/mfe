context("Itemset meta-features")

test_that("itemset.result", {
  aux = itemset(Species ~ ., iris)
  
  expect_named(aux, ls.itemset())
  expect_equal(aux, itemset(iris[1:4], iris[5]))
  expect_named(itemset(Species ~ ., iris, ls.itemset()[1:2]), 
               ls.itemset()[1:2])
})

test_that("itemset.errors",{
  expect_error(itemset(iris[1:130, 1:4], iris[5]))
  expect_error(itemset(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(itmset(Species ~ ., iris, features=c("abc", "xdef")))
})
