context("Model based meta-features")

set.seed(1)
aux <- rnorm(100)
rmfdata <- data.frame(
  n1 = aux * (rnorm(10) / 10),
  n2 = 1:100,
  n3 = aux,
  n4 = sample(1:2, 100, TRUE),
  class = rep(factor(c(1,1,1,1,1,2,2,2,2,2)), 10)
)

test_that("mf.model.based.result", {
  features <- mf.model.based(class ~ ., rmfdata)
  expect_named(features, ls.model.based())
  expect_equal(features, mf.model.based(class ~ ., rmfdata, ls.model.based()))
  expect_equal(features, mf.model.based(rmfdata[1:4], rmfdata[5]))

  expect_length(features, length(ls.model.based()))
  attrs <- c("homogeneity", "branch.length", "nodes.per.level", "depth")
  expect_true(all(sapply(features[attrs], length) == 2))

  attrs <- c("max.depth", "nodes.per.attribute", "nnode", "nodes.per.instance")
  features <- mf.model.based(class ~ ., rmfdata, attrs)
  expect_is(features, "list")
  expect_named(features, attrs)
  expect_true(all(sapply(features, length) == 1))

  one <- mf.model.based(iris[1:4], iris$Species, ls.model.based()[1])
  expect_is(one, "list")
  expect_named(one, ls.model.based()[1])
})

test_that("mf.model.based.errors",{
   #Test erros cases
   expect_error(mf.model.based(rmfdata[1:9, c(1,2)], rmfdata[4]),
                "x and y must have same number of rows")
   expect_error(mf.model.based(as.matrix(rmfdata[, c(1,2)]), rmfdata[3]),
                "data argument must be a data.frame")
   expect_error(mf.model.based.formula(rmfdata[, c(1,2)], rmfdata[3]),
                "method is only for formula datas")
   expect_error(mf.model.based(class ~ ., rmfdata, features=c("abc", "def")))
})
