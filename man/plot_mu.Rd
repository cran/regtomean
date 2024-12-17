\name{plot_mu}
\alias{plot_mu}
\title{Plot t-Statistics and p-Values for Intervention Impact}
\usage{
plot_mu(x, n, se_after, lower = F, alpha = 0.05)
}
\arguments{
\item{x}{A data frame containing the results from \code{meechua_reg}. Specifically, this should be the \code{mod_coef} data frame obtained from \code{meechua_reg}.}

\item{n}{The original sample size (number of observations) of the data.}

\item{se_after}{The estimated standard error from \code{meechua_reg}. This should be the \code{se_after} vector obtained from \code{meechua_reg}.}

\item{lower}{A boolean value specifying the direction of the one-sided tests. For \code{lower = FALSE} (the default), it tests whether the intervention is increasing the measurements. For \code{lower = TRUE}, it tests whether the second measurements are lower than expected.}

\item{alpha}{Specifies the significance threshold for the p-values of the corresponding one-sided tests. The default is \code{alpha = 0.05}.}
}
\value{
A list containing the most significant \eqn{\mu}, t-statistic, p-value, and the range of \eqn{\mu} for which the treatment impact is significant.
}
\description{
Based on the data before and after the intervention and the regression models from the function \code{meechua_reg}, this function plots the t-statistics and p-values for a given range of \eqn{\mu} to assess whether the intervention has a significant impact on the measurements, accounting for regression to the mean.
}
\examples{
# Generate example data
language_test <- data.frame(
  Before = rnorm(100, mean = 50, sd = 10),
  After = rnorm(100, mean = 55, sd = 10)
)

mee_chua <- replicate_data(0, 1, "Before", "After", data = language_test)
mee_chua <- mee_chua[order(mee_chua$mu), ]  # Sortieren nach 'mu'

# Regression ausführen und Ergebnisse erhalten
reg_results <- meechua_reg(mee_chua)

# Zugriff auf Ergebnisse
mod_coef <- reg_results$mod_coef
se_after <- reg_results$se_after

# mod_coef and se_after are stored in the environment.
plot_mu(mod_coef, 8, se_after)
}
\references{
Ostermann, T., Willich, S. N., & Luedtke, R. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.
}
\author{
Julian Stein
}
