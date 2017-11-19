context("run basic tests")

test_that("main function works",{

  p <- ggparci(iris[,-5], iris[,5])
  expect_s3_class(p,"ggplot")
})
