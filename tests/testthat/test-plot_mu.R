library(testthat)
library(regtomean)

test_that("plot_mu returns valid results", {
  mod_coef <- data.frame(
    V1 = seq(10, 1000, length.out = 100),
    V2 = seq(20, 1010, length.out = 100)
  )
  se_after <- rep(5, 100)
  
  results <- plot_mu(mod_coef, n = 8, se_after = se_after)
  
  # Dynamische Validierungen basierend auf den Eingaben
  expect_true(is.finite(results$t_opt))        # `t_opt` sollte einen endlichen Wert haben
  expect_true(results$p_min >= 0 && results$p_min <= 1)  # `p_min` sollte ein gültiger p-Wert sein
  expect_true(results$mu_max >= min(mod_coef$V1 / 100))  # `mu_max` sollte im Bereich der Eingaben liegen
  expect_true(results$mu_lower <= results$mu_max) # `mu_lower` und `mu_max` sollten sinnvoll sein
})