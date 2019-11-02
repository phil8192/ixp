context("util")

test_that("roc works", {
  roc_res = roc(c(23, 10, 50, 100, 110))
  exp_res = c(NA, -0.5652174, 4, 1, 0.1)
  res_diff <- abs(sum(roc_res - exp_res, na.rm=T))
  expect_true(res_diff < 10^-8)
})
