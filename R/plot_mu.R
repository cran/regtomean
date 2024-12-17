#' @title Plot t-Statistics and p-Values for Intervention Impact
#' @description Based on the data before and after the intervention and the regression models from the function \code{meechua_reg}, this function plots the t-statistics and p-values for a given range of \eqn{\mu} to assess whether the intervention has a significant impact on the measurements, accounting for regression to the mean.
#' @param x A data frame containing the results from \code{meechua_reg}. Specifically, this should be the \code{mod_coef} data frame obtained from \code{meechua_reg}.
#' @param n The original sample size (number of observations) of the data.
#' @param se_after The estimated standard error from \code{meechua_reg}. This should be the \code{se_after} vector obtained from \code{meechua_reg}.
#' @param lower A boolean value specifying the direction of the one-sided tests. For \code{lower = FALSE} (the default), it tests whether the intervention is increasing the measurements. For \code{lower = TRUE}, it tests whether the second measurements are lower than expected.
#' @param alpha Specifies the significance threshold for the p-values of the corresponding one-sided tests. The default is \code{alpha = 0.05}.
#' @return A list containing the most significant \eqn{\mu}, t-statistic, p-value, and the range of \eqn{\mu} for which the treatment impact is significant.
#' @references Ostermann, T., Willich, S. N., & Luedtke, R. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.
#' @author Julian Stein
#' @examples
#' # First perform replicate_data and meechua_reg
#' replicate_data(0, 100, "Before", "After", data=language_test)
#' meechua_reg(mee_chua)
#' # mod_coef and se_after are stored in the environment.
#' plot_mu(mod_coef, 8, se_after)
#' @export
#' @import ggplot2 

plot_mu <- function(x, n, se_after, lower = F, alpha = 0.05) {
  
  e.mu <- x[,1] / 100
  treatment <- x[,2] - x[,1] / 100
  t <- treatment / se_after
  p <- pt(t, df = n - 2, lower.tail = lower)
  
  # Adjusting t-value axis
  t_min <- min(t)
  t_max <- max(t)
  t_range <- t_max - t_min
  t_rescaled <- (t - t_min) / t_range
  
  # Calculate key statistics
  t_opt <- if (lower) t_min else t_max
  p_min <- min(p)
  filtered_mu <- e.mu[p <= alpha]
  mu_upper <- if (length(filtered_mu) == 0) -Inf else max(filtered_mu)
  mu_lower <- if (length(filtered_mu) == 0) Inf else min(filtered_mu)
  opt_index <- which.min(p)
  mu_max <- e.mu[opt_index]
  
  # Print relevant values
  cat("Results:\n")
  cat("t opt:   ", sprintf("%-9.4f", t_opt), "in absolute values the best possible t-statistic to test for a treatment effect\n")
  cat("p min:   ", sprintf("%-9.4f", p_min), "the minimal p-value testing for a treatment effect\n")
  cat("\u03bc max:   ", sprintf("%-9.2f", mu_max), "that \u03bc where the p-value is minimal and treatment effect is 'maximally significant'\n")
  cat("\u03bc lower: ", sprintf("%-9.2f", mu_lower), "(lowest value of \u03bc with a p-value \u2264", alpha, ")\n")
  cat("\u03bc upper: ", sprintf("%-9.2f", mu_upper), "(highest value of \u03bc with a p-value \u2264", alpha, ")\n\n")
  
  if (is.infinite(mu_lower) && mu_lower > 0) {
    cat("In the provided range of \u03bc there is no \u03bc for which the test result is significant according to \u03B1.\n")
  } else {
    cat("For \u03bc \u2208 [", mu_lower, ", ", mu_upper, "], treatment effect is significant under \u03B1, considering regression to the mean.\n")
  }
  
  results <- data.frame(e.mu = e.mu, p = p, t_rescaled = t_rescaled)
  
  # Only create and print the plot in interactive mode
  if (interactive()) {
    base_plot <- ggplot(results, aes(x = e.mu)) +
      geom_line(aes(y = p), color = "blue", linewidth = 1.5) +
      geom_line(aes(y = t_rescaled), color = "red", linewidth = 1.5) +
      geom_hline(yintercept = alpha, linetype = "dashed", color = "blue", linewidth = 1) +
      labs(x = expression(mu), y = "one sided p-value") +
      scale_y_continuous(
        breaks = seq(0, 1, by = 0.05),
        expand = c(0, 0),
        name = "one sided p-value",
        sec.axis = sec_axis(~ . * t_range + t_min, name = "t-statistic")
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      theme_minimal()
    
    # Add annotations
    base_plot <- base_plot +
      annotate("segment", x = -Inf, xend = Inf, y = 0, yend = 0, color = "black", linewidth = 1) +
      annotate("segment", x = -Inf, xend = Inf, y = 1, yend = 1, color = "black", linewidth = 1) +
      annotate("segment", x = min(e.mu), xend = min(e.mu), y = -Inf, yend = Inf, color = "blue", linewidth = 1) +
      annotate("segment", x = max(e.mu), xend = max(e.mu), y = -Inf, yend = Inf, color = "red", linewidth = 1)
    
    print(base_plot)
  }
  
  return(invisible(list(t_opt = t_opt, p_min = p_min, mu_max = mu_max, mu_lower = mu_lower, mu_upper = mu_upper)))
}