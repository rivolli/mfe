context("Post-processing")

test_that("post.processing", {
  expect_equal(post.processing(1), c(1))
  expect_equal(post.processing(1:10, c("min", "max")), c(min=1, max=10))

  #Non.aggregating
  expect_equal(post.processing(1:10, "non.aggregated"), c(1:10), check.names=FALSE)

  #Mode
  expect_equal(post.processing(c(1,1,2,2,2,4,4,4,4), "mode"), c(mode=4))

  #Kurtosis and Skewness
  set.seed(1)
  my.seq <- round((runif(100) + runif(100)) * 10)
  expected <- c(
    kurtosis = e1071::kurtosis(my.seq),
    skewness = e1071::skewness(my.seq)
  )
  expect_equal(post.processing(my.seq, names(expected)), expected)

  #Histogram
  expect_equal(post.processing(1:10, "hist"), rep(0.1, 10), check.names=FALSE)
  expect_equal(post.processing(1:10, "hist", max=20),
               c(rep(0.2, 5), rep(0, 5)), check.names=FALSE)
  expect_equal(post.processing(1:10, "hist", min=-10, bins=2), c(hist1=0, hist2=1))
})

test_that("different function", {
  #TODO scope problems
  #my.test.func <- function (x, ...) sum(x)
  #expect_equal(post.processing(1:10, "my.test.func"), sum(1:10))
})

test_that("error scenarios", {
  #Additional arguments
  expect_equal(post.processing(c(1:10, NA), "mean"), c(mean=mean(NA)))
  expect_equal(post.processing(c(1:10, NA), "mean", na.rm = TRUE),
               c(mean=mean(1:10)))

  #Multiples arguments
  res <- post.processing(c(1:10, NA), c("mean", "skewness"), na.rm=TRUE, type=2)
  expect_equal(res, c(mean=mean(1:10), skewness=0))

  res <- post.processing(c(1:10), c("mean", "max", "skewness", "hist"),
                         bins=5, na.rm=TRUE, type=2)
  expect_length(res, 8)
})
