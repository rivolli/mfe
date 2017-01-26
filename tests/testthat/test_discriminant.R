context("Discriminant meta-features")

set.seed(1)
aux <- rnorm(100)
rmfdata <- data.frame(
  n1 = aux * (rnorm(10) / 10),
  n2 = 1:100,
  n3 = aux,
  n4 = sample(1:2, 100, TRUE),
  class = rep(factor(c(1,1,1,1,1,2,2,2,2,2)), 10)
)

test_that("mf.discriminant.result", {
  features <- mf.discriminant(class ~ ., rmfdata)
  expect_named(features, ls.discriminant())
  expect_equal(features, mf.discriminant(class ~ ., rmfdata, ls.discriminant()))
  expect_equal(features, mf.discriminant(rmfdata[1:4], rmfdata[5]))

  expect_length(features, length(ls.discriminant()))
  expect_true(all(sapply(features, length) == 1))

  attrs <- c("cancor.fract", "sdratio", "wlambda", "cancor")
  features <- mf.discriminant(class ~ ., rmfdata, attrs)
  expect_named(features, attrs)
  expect_true(all(sapply(features, length) == 1))

  one <- mf.discriminant(iris[1:4], iris$Species, ls.discriminant()[1])
  expect_is(one, "list")
  expect_named(one, ls.discriminant()[1])
})

test_that("mf.discriminant.errors",{
   #Test erros cases
   expect_error(mf.discriminant(rmfdata[1:9, c(1,2)], rmfdata[4]),
                "x and y must have same number of rows")
   expect_error(mf.discriminant(as.matrix(rmfdata[, c(1,2)]), rmfdata[3]),
                "data argument must be a data.frame")
   expect_error(mf.discriminant.formula(rmfdata[, c(1,2)], rmfdata[3]),
                "method is only for formula datas")
   expect_error(mf.discriminant(class ~ ., rmfdata, features=c("abc", "def")))
})
