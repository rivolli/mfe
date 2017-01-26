context("Statistical meta-features")

set.seed(1)
aux <- rnorm(10)
rmfdata <- data.frame(
  n1 = aux * (rnorm(10) / 10),
  n2 = 1:10,
  n3 = aux,
  class = factor(c(1,1,1,1,1,2,2,2,2,2))
)

test_that("mf.statistical.result", {
  features <- mf.statistical(class ~ ., rmfdata)
  expect_named(features, ls.statistical())
  expect_equal(features, mf.statistical(class ~ ., rmfdata, ls.statistical()))
  expect_equal(features, mf.statistical(rmfdata[c(1,2,3)], rmfdata[4]))

  expect_length(features, length(ls.statistical()))
  expect_true(all(sapply(features, length) == 2))
  expect_true(all(sapply(features, names) == c("mean", "sd")))

  new.features <- mf.statistical(class ~ ., rmfdata, by.class = FALSE)
  f <- setdiff(ls.statistical(), c("harmonic.mean", "outliers", "normality", "discreteness.degree"))
  expect_false(any(do.call(rbind, features[f]) == do.call(rbind, new.features[f])))
  expect_length(new.features$outliers, 1)

  features <- mf.statistical(class ~ ., rmfdata, c("variance", "skewness"))
  expect_named(features, c("variance", "skewness"))

  features <- mf.statistical(class ~ ., rmfdata,
                             c("variance", "skewness", "iqr", "covariance"),
                             summary="min")
  expect_named(features, c("variance", "skewness", "iqr", "covariance"))
  expect_true(all(sapply(features, names) == "min"))

  one <- mf.statistical(iris[1:4], iris$Species, ls.statistical()[1])
  expect_is(one, "list")
  expect_named(one, ls.statistical()[1])

})

test_that("mf.statistical.values", {
  data <- rmfdata[1:3]

  #With class information
  features <- mf.statistical(class ~ ., rmfdata, "variance", summary=c("min", "max"))
  expected <- range(c(apply(data[1:5,], 2, var), apply(data[6:10,], 2, var)))
  names(expected) <- c("min", "max")
  expect_equal(features$variance, expected)

  #Without class information
  features <- mf.statistical(class ~ ., rmfdata, summary=c("mean", "max"), by.class=FALSE)
  expect_equal(features$standard.deviation[[1]], mean(apply(data, 2, sd)))
  expect_equal(features$variance[[1]], mean(apply(data, 2, var)))
  expect_equal(features$kurtosis[[1]], mean(abs(apply(data, 2, e1071::kurtosis))))
  expect_equal(features$skewness[[1]], mean(abs(apply(data, 2, e1071::skewness))))

  expect_equal(features$covariance[[2]], max(abs(stats::cov(data))[c(2,3,6)]))
  expect_equal(features$correlation[[2]], max(abs(stats::cor(data))[c(2,3,6)]))

  #TODO variance.fraction.1d
})

test_that("mf.statistical.errors",{
  #Test erros cases
  expect_error(mf.statistical(rmfdata[1:9, c(1,2)], rmfdata[4]),
               "x and y must have same number of rows")
  expect_error(mf.statistical(as.matrix(rmfdata[, c(1,2)]), rmfdata[3]),
               "data argument must be a data.frame")
  expect_error(mf.statistical.formula(rmfdata[, c(1,2)], rmfdata[3]),
               "method is only for formula datas")
  expect_error(mf.statistical(class ~ ., rmfdata, features=c("abc", "def")))
})


test_that("mf.statistical.strange.things", {
  #Constant value for each label
  rmfdata$n4 <- c(rep(1, 5), rep(2, 5))
  #TODO expect_silent(mf.statistical(class ~ ., rmfdata))
})
