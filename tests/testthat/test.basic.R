context("run basic tests")

test_that("main function works",{

  library(ggplot2)
  p <- ggparci(iris, groups_column = "Species")
  expect_s3_class(p,"ggplot")
})
