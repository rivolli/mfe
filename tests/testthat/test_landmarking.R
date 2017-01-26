context("Landmarking meta-features")

set.seed(1)
aux <- rnorm(100)
rmfdata <- data.frame(
  n1 = aux * (rnorm(10) / 10),
  n2 = 1:100,
  n3 = aux,
  n4 = sample(1:2, 100, TRUE),
  class = rep(factor(c(1,1,1,1,1,2,2,2,2,2)), 10)
)

test_that("mf.landmarking.result", {
  features <- mf.landmarking(class ~ ., rmfdata)
  expect_named(features, ls.landmarking())
  expect_equal(features, mf.landmarking(class ~ ., rmfdata, ls.landmarking()))
  expect_equal(features, mf.landmarking(rmfdata[1:4], rmfdata[5]))

  expect_length(features, length(ls.landmarking()))
  expect_true(all(sapply(features, length) == 2))

  attrs <- c("naive.bayes", "linear.discriminant", "worst.node")
  features <- mf.landmarking(Species ~ ., iris, attrs)
  expect_is(features, "list")
  expect_named(features, attrs)

  one <- mf.landmarking(iris[1:4], iris$Species, ls.landmarking()[1])
  expect_is(one, "list")
  expect_named(one, ls.landmarking()[1])
})

test_that("mf.landmarking.errors",{
   #Test erros cases
   expect_error(mf.landmarking(rmfdata[1:9, c(1,2)], rmfdata[4]),
                "x and y must have same number of rows")
   expect_error(mf.landmarking(as.matrix(rmfdata[, c(1,2)]), rmfdata[3]),
                "data argument must be a data.frame")
   expect_error(mf.landmarking.formula(rmfdata[, c(1,2)], rmfdata[3]),
                "method is only for formula datas")
   expect_error(mf.landmarking(class ~ ., rmfdata, features=c("abc", "def")))
   expect_error(mf.landmarking(class ~ ., rmfdata, map="abc"))
   expect_error(mf.landmarking(class ~ ., rmfdata, folds=0))
})
