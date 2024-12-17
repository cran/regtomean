#' Plot Results for p-values and t-values
#'
#' This function plots the t-statistics and p-values for a range of \eqn{\mu} values, based on the provided data and regression models. It helps visualize whether the intervention has a significant impact on the measurements, accounting for regression to the mean.
#'
#' @param mu_start Numeric. The starting value of \eqn{\mu} for the range of values to be plotted.
#' @param mu_end Numeric. The ending value of \eqn{\mu} for the range of values to be plotted.
#' @param n Numeric. The original sample size (number of observations) of the data.
#' @param y1_mean Numeric. The mean of the first measurement.
#' @param y2_mean Numeric. The mean of the second measurement.
#' @param y1_std Numeric. The standard deviation of the first measurement.
#' @param y2_std Numeric. The standard deviation of the second measurement.
#' @param cov Numeric. The covariance between the two measurements, or if \code{r_insteadof_cov} is \code{TRUE}, the correlation coefficient.
#' @param lower Logical. If \code{TRUE}, the function tests whether the second measurements are lower than expected. If \code{FALSE} (the default), it tests whether the intervention is increasing the measurements.
#' @param alpha Numeric. The significance threshold for the p-values of the one-sided tests. The default is \code{0.05}.
#' @param r_insteadof_cov Logical. If \code{TRUE}, \code{cov} is interpreted as the correlation coefficient instead of the covariance. Default is \code{FALSE}.
#'
#' @return A \code{ggplot2} plot with two y-axes: one showing p-values and the other showing t-statistics. The function also prints key values including the most significant \eqn{\mu}, the minimal p-value, and the range of \eqn{\mu} where the treatment effect is significant.
#' @references Ostermann, T., Willich, S. N., & Luedtke, R. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.
#' @author Julian Stein
#' @examples
#' # Example usage of the plot_t function
#' plot_t(
#'   mu_start = 0, mu_end = 10, n = 50, y1_mean = 5, 
#'   y2_mean = 5, y1_std = 2, y2_std = 2, cov = 0.5
#' )
#' 
#' plot_t(
#'   mu_start = 0, mu_end = 10, n = 50, y1_mean = 5, 
#'   y2_mean = 5, y1_std = 2, y2_std = 2, cov = 0.5, 
#'   lower = TRUE, alpha = 0.1
#' )
#' 
#' @export
#' @import ggplot2

plot_t <- function(mu_start, mu_end, n, y1_mean, y2_mean, y1_std, y2_std, cov, lower = F, alpha = 0.05, r_insteadof_cov = F) {
  if (r_insteadof_cov) {
    r <- cov
    cov <- r * y1_std * y2_std
  } else {
    r <- cov / (y1_std * y2_std)
  }
  
  e.mu <- seq(from = mu_start, to = mu_end, length.out = 101)
  numerator <- sqrt(n * (n - 2)) * (y1_std^2 * y2_mean - cov * y1_mean + (cov - y1_std^2) * e.mu)
  denominator <- sqrt((y1_std^2 * y2_std^2 - cov^2) * ((n - 1) * y1_std^2 + n * (y1_mean - e.mu)^2))
  t <- numerator / denominator
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
    
    print(base_plot)
  }
  
  return(invisible(list(t_opt = t_opt, p_min = p_min, mu_max = mu_max, mu_lower = mu_lower, mu_upper = mu_upper)))
}