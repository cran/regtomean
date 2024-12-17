#################################################################################
# calculating treatment effects, regression effects and p-values;
#' @export
#' @import stats
#' @import plotrix
#' @import graphics

meechua_eff.CI <- function(x, n, se_after) {
  # Überprüfe Eingabedaten
  if (nrow(x) == 0 || length(se_after) == 0) {
    stop("Input data is empty. Please check x and se_after.")
  }
  if (any(is.na(x)) || any(is.na(se_after))) {
    stop("Input data contains NA values.")
  }
  
  # Berechnungen
  e.mu <- x[, 1] / 100
  treatment <- x[, 2] - x[, 1] / 100
  t <- treatment / se_after
  m <- mean(treatment, na.rm = TRUE)
  error <- qt(0.975, df = n - 2) * se_after
  lower <- treatment - error
  upper <- treatment + error
  p <- pt(t, df = n - 2, lower.tail = FALSE)
  
  # Debugging-Ausgaben
  cat("\nDebugging information:\n")
  cat("e.mu:\n")
  print(e.mu)
  cat("treatment:\n")
  print(treatment)
  cat("p:\n")
  print(p)
  cat("lower:\n")
  print(lower)
  cat("upper:\n")
  print(upper)
  
  # Plots nur erstellen, wenn NICHT in einer CRAN-Umgebung
  if (Sys.getenv("NOT_CRAN", unset = "TRUE") == "TRUE") {
    
  twoord.plot(
    e.mu, treatment, e.mu, p,
    lylim = range(treatment, na.rm = TRUE),
    rylim = c(0, 1),
    lytickpos = pretty(range(treatment, na.rm = TRUE)),
    rytickpos = seq(0, 1, by = 0.2),
    main = "Treatment Effect and p-value",
    ylab = "Treatment Effect",
    rylab = "p-value",
    type = c("l", "l"),
    xlab = "mu"
  )
  
  plotCI(
    e.mu, treatment, error,
    xlab = "mu",
    pt.bg = par("bg"),
    pch = 21,
    lwd = 2,
    scol = "gray",
    main = "Confidence Intervals"
  )
  } else {
    message("Plots are not generated during CRAN checks.")
  }
  
  # Rückgabe der berechneten Werte
  return(invisible(list(treatment = treatment, p = p, lower = lower, upper = upper, t = t)))
}
