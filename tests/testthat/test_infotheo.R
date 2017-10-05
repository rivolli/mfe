context("Information-theoretic meta-features")

set.seed(1)
aux <- rnorm(100)
rmfdata <- data.frame(
  n1 = aux * (rnorm(10) / 10),
  n2 = 1:100,
  n3 = aux,
  n4 = sample(1:2, 100, TRUE),
  class = rep(factor(c(1,1,1,1,1,2,2,2,2,2)), 10)
)

test_that("mf.infotheo.result", {
  features <- mf.infotheo(class ~ ., rmfdata)
  expect_named(features, ls.infotheo())
  expect_equal(features, mf.infotheo(class ~ ., rmfdata, ls.infotheo()))
  expect_equal(features, mf.infotheo(rmfdata[1:4], rmfdata[5]))

  expect_length(features, length(ls.infotheo()))

  mattrs <- c("attributes.concentration", "attribute.entropy",
             "class.concentration", "joint.entropy", "mutual.information")
  expect_true(all(sapply(features[mattrs], length) == 2))
  expect_true(all(sapply(features[mattrs], names) == c("mean", "sd")))

  attrs <- c("class.entropy", "equivalent.attributes", "noise.signal")
  features <- mf.infotheo(class ~ ., rmfdata, attrs)
  expect_named(features, attrs)
  expect_true(all(sapply(features, length) == 1))

  features <- mf.infotheo(class ~ ., rmfdata, summary="min")
  expect_true(all(sapply(features[mattrs], names) == "min"))

  one <- mf.infotheo(iris[1:4], iris$Species, ls.infotheo()[1])
  expect_is(one, "list")
  expect_named(one, ls.infotheo()[1])
})

test_that("mf.infotheo.values", {
  #TODO
})

test_that("mf.infotheo.errors",{
  #Test erros cases
  expect_error(mf.infotheo(rmfdata[1:9, c(1,2)], rmfdata[4]),
               "x and y must have same number of rows")
  expect_error(mf.infotheo(as.matrix(rmfdata[, c(1,2)]), rmfdata[3]),
               "data argument must be a data.frame")
  expect_error(mf.infotheo.formula(rmfdata[, c(1,2)], rmfdata[3]),
               "method is only for formula datas")
  expect_error(mf.infotheo(class ~ ., rmfdata, features=c("abc", "def")))
})
