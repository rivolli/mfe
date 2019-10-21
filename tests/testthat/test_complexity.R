context("Complexity meta-features")

test_that("complexity.result", {
  aux = complexity(Species ~ ., iris)
  
  #L3 and N4 are non deterministic
  dets <- c("C1", "C2", "ClsCoef", "Density", "F1", "F1v", "F2", "F3", "F4", 
            "Hubs", "L1", "L2", "LSC", "N1", "N2", "N3", "T1", "T2", "T3", "T4")
  expect_named(aux, ls.complexity())
  expect_equal(aux[dets], complexity(iris[1:4], iris[5], dets))
  expect_named(complexity(Species ~ ., iris, ls.complexity()[1:3]), 
               ls.complexity()[1:3])
})

test_that("complexity.errors",{
  expect_error(complexity(iris[1:130, 1:4], iris[5]))
  expect_error(complexity(as.matrix(iris[, c(1,2)]), iris$Species))
  expect_error(complexity(Species ~ ., iris, features=c("abc", "xdef")))
})
