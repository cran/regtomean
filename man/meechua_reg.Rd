\name{meechua_reg}
\alias{meechua_reg}
\title{Fit linear models on the (replication) data.}
\description{This function fit linear models for a subset of data frames.}
\usage{meechua_reg(x)}
\arguments{
  \item{x}{Data to be used in the regression.}
}
\details{
The data used for the regression must be sorted by \code{mu}.

A set of linear \code{models} will be estimated and model coefficients are saved and stored in \code{mod_coef}.

The estimated standard errror for the \code{after} measure is also stored in \code{se_after} to be used further in other functions.}
\value{
A table containing the estimations for each \code{mu}.
The variables \code{models}, \code{mod_coef}, \code{se_after} are stored globally for further analysis if \code{to_global} is set to TRUE. In any case the values will be returned.
The models are saved in an object called \code{mee_chua}, which is not automatically printed but is saved in the environment.}
\references{
Ostermann, T., Willich, Stefan N. & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.}
\author{Daniela Recchia, Thomas Ostermann.}

\seealso{\link[stats]{lm},\link[plyr]{dlply}}
\examples{
# Generate example data
language_test <- data.frame(
  Before = rnorm(100, mean = 50, sd = 10),
  After = rnorm(100, mean = 55, sd = 10)
)

# Replicate data
replicate_data <- function(start, end, Before, After, data) {
  mu <- seq(start * 100, end * 100, by = (end - start))
  mu <- rep(mu, each = nrow(data))
  
  before <- data[[Before]] - mu / 100
  after <- data[[After]]
  
  mee_chua <- data.frame(mu = mu, before = before, after = after)
  return(mee_chua)
}

mee_chua <- replicate_data(0, 1, "Before", "After", data = language_test)
mee_chua <- mee_chua[order(mee_chua$mu), ]  # Sortieren nach 'mu'

# Regression ausführen und Ergebnisse erhalten
reg_results <- meechua_reg(mee_chua)

# Zugriff auf Ergebnisse
mod_coef <- reg_results$mod_coef
se_after <- reg_results$se_after

# Anzeigen der Ergebnisse
print(mod_coef)
print(se_after)
}

