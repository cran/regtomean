\name{meechua_plot}
\alias{meechua_plot}
\title{Plot models from \code{meechua_reg}}
\description{This functions plots all 4 diagnostics plots for each linear regression model: "Residuals vs Fitted", "Normal Q-Q", "Scale-Location" and "Residuals vs Leverage".}
\usage{meechua_plot(models = NULL, env = regtomean_env)}
\arguments{
  \item{models}{
    A list containing the estimated linear models, typically the output of \code{meechua_reg}.
    If \code{models} is \code{NULL}, the function attempts to retrieve the models from the specified environment (\code{env}).
  }
  \item{env}{
    An environment where the models are stored. The default is \code{regtomean_env}.
    This argument is used only if \code{models} is not explicitly provided.
  }
}
\details{For each model from \code{models} 4 diagnostic plots are performed. For the first model the numbers 1 to 4 should be given, for the second model numbers from  to 8 to 12, and so on.}
\value{Diagnostics plots for the set of models from \code{meechua_reg}.}
\references{
Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.}
\author{Daniela Recchia, Thomas Ostermann.}

\seealso{\link[stats]{plot.lm},\link[regtomean]{meechua_reg}}
\examples{
# Generate example data
language_test <- data.frame(
  Before = rnorm(100, mean = 50, sd = 10),
  After = rnorm(100, mean = 55, sd = 10)
)

# Replicate data
mee_chua <- replicate_data(50, 60, "Before", "After", data = language_test)
mee_chua_sort <- mee_chua[order(mee_chua$mu), ]

# Perform regression analysis
results <- meechua_reg(mee_chua_sort)

# Plot models
meechua_plot(results$models)}

