\name{meechua_eff.CI}
\alias{meechua_eff.CI}
\title{Calculates and plots treatment and regression effects as also its p-values.}
\description{This function calculates and plots treatment and regression effects of both before and after measures as also its p-values.}
\usage{meechua_eff.CI(x,n,se_after)}
\arguments{
  \item{x}{a data frame containing the results from \code{meechua_reg}. It is stored as \code{mod_coef}.}
  \item{n}{the original sample size (number of observations) from data.}
  \item{se_after}{the estimated standard error from \code{meechua_reg}. It is stored as \code{se_after}.}}
\details{After performing the \code{meechua_reg} the model coefficients \code{mod_coef} as also its global variable \code{se_after} are used as input in this function to estimate treatment and regression effects.}
\value{Two plots are performed, the first "Treatment Effect and p-value" and the second "Confidence Intervals" for \code{mu}.}
\references{Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.}
\author{Daniela Recchia, Thomas Ostermann}
\seealso{\link[regtomean]{meechua_reg}}
\examples{
# Initialize environment explicitly
#regtomean_env <- new.env(parent = emptyenv())

# Generate example data
language_test <- data.frame(
  Before = rnorm(100, mean = 50, sd = 10),
  After = rnorm(100, mean = 55, sd = 10)
)

# Replicate data
mee_chua <- replicate_data(0, 100, "Before", "After", data = language_test)
mee_chua <- mee_chua[order(mee_chua$mu), ]

# Perform regression analysis and store results
results <- meechua_reg(mee_chua)
mod_coef <- results$mod_coef
se_after <- results$se_after

# Call meechua_eff.CI
meechua_eff.CI(mod_coef, 100, se_after)}