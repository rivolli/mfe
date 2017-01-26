context("General meta-features")

set.seed(1)
rmfdata <- data.frame(
  n1 = rnorm(100),
  n2 = sample(1:2, 100, TRUE),
  c1 = factor(sample(1:2, 100, TRUE)),
  c2 = factor(sample(1:5, 100, TRUE))
)

test_that("mf.general.result", {
  features <- mf.general(c1 ~ ., rmfdata)
  expect_named(features, ls.general())
  expect_equal(features, mf.general(c1 ~ ., rmfdata, ls.general()))
  expect_equal(features, mf.general(rmfdata[c(1,2,4)], rmfdata[3]))

  #Test list format
  expect_is(features, "list")
  expect_is(mf.general(c1 ~ ., rmfdata, ls.general()[1]), "list")
  expect_is(mf.general(c1 ~ ., rmfdata, ls.general()[1:2]), "list")
  expect_is(mf.general(c1 ~ ., rmfdata, ls.general()[1:3]), "list")
  expect_is(mf.general(c1 ~ ., rmfdata, ls.general()[1:4]), "list")

  one <- mf.general(iris[1:4], iris$Species, ls.general()[1])
  expect_is(one, "list")
  expect_named(one, ls.general()[1])
})

test_that("mf.general.values", {
  features <- mf.general(c1 ~ ., rmfdata)

  #Check expected values
  expected <- list(
    defective.instances=0, dimensionality=3/100,
    majority.class=max(summary(rmfdata$c1)) / 100, missing.values=0,
    nattribute=3, nbinary=1, nclasse=2, ninstance=100, nnumeric=2, nsymbolic=1,
    pbinary=1/3, pnumeric=2/3, psymbolic=1/3,
    sdclass=sd(table(rmfdata$c1) / 100)
  )
  expect_equal(features, expected)

  #Change the class
  features <- mf.general(c2 ~ ., rmfdata)
  expected$majority.class <- max(summary(rmfdata$c2)) / 100
  expected$nbinary <- 2
  expected$pbinary <- 2/3
  expected$nclasse <- 5
  expected$sdclass <- sd(table(rmfdata$c2) / 100)
  expect_equal(features, expected)
})

test_that("mf.general.errors",{
  #Test erros cases
  expect_error(mf.general(rmfdata[1:10, c(1,2,4)], rmfdata[3]),
               "x and y must have same number of rows")
  expect_error(mf.general(as.matrix(rmfdata[, c(1,2)]), rmfdata[3]),
               "data argument must be a data.frame")
  expect_error(mf.general.formula(rmfdata[, c(1,2)], rmfdata[3]),
               "method is only for formula datas")
  expect_error(mf.general(n1 ~ ., rmfdata, features=c("abc", "xdef")))
})
