################## # regression analysis for each mu ############################
#' @export
#' @import stats
### data must be ordered by mu ###


meechua_reg <- function(x) {
  models <- lapply(split(x, x$mu), function(df) lm(after ~ before, data = df))
  mod_coef <- do.call(rbind, lapply(models, coef))
  mod_coef <- as.data.frame(mod_coef, stringsAsFactors = FALSE)
  
  results <- do.call(rbind, lapply(models, function(i) coef(summary(i))))
  se <- results[, "Std. Error"]
  se_after <- se[seq(1, length(se), 2)]
  
  # Direkte Rückgabe der Ergebnisse
  return(list(models = models, mod_coef = mod_coef, se_after = se_after))
}