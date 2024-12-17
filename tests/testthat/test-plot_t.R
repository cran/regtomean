library(testthat)
library(regtomean)

test_that("plot_t returns valid results", {
  results <- plot_t(
    mu_start = 0,
    mu_end = 100,
    n = 8,
    y1_mean = 57.375,
    y2_mean = 60.375,
    y1_std = 7.0,
    y2_std = 8.8,
    cov = 54.268
  )
  
  expect_true(is.numeric(results$t_opt))        # `t_opt` sollte numerisch sein
  expect_true(results$p_min >= 0 && results$p_min <= 1) # `p_min` sollte ein gültiger p-Wert sein
  expect_true(results$mu_max >= 0)              # `mu_max` sollte sinnvoll sein
  expect_true(is.infinite(results$mu_lower) || results$mu_lower <= results$mu_max)
  expect_true(is.infinite(results$mu_upper) || results$mu_upper >= results$mu_lower)
})