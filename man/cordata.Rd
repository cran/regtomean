\name{cordata}
\alias{cordata}
\title{Correlation and Cohen's d effect sizes.}
\description{This function calculates the correlation for the data and Cohen's d effect sizes, both based on pooled and on treatment standard deviations. It can optionally display the results in an HTML widget.}
\usage{cordata(Before, After, within = TRUE, data = NULL)}
\arguments{
  \item{Before}{a numeric vector giving the data values for the first (before) measure.}
  \item{After}{a numeric vector giving the data values for the second (after) measure.}
  \item{within}{A logical indicating whether the effect sizes should be computed based on paired samples (\code{TRUE}, default) or independent samples (\code{FALSE}).}
  \item{data}{an optional data frame containing the variables in the formula. By \code{default} the variables are taken from \code{environment (formula)}.}
}
\details{This function computes the correlation between two measures and calculates Cohen's d effect sizes using both pooled and treatment standard deviations. 

  - If \code{within = TRUE}, the effect sizes are computed assuming paired samples.
  - If \code{within = FALSE}, the effect sizes are computed assuming independent samples.

  The results are returned as a data frame and also displayed in an HTML widget in the RStudio Viewer or default web browser.}
\value{Return a table containing the correlation, effect size pooled and effect size based on treatment.}
\references{
Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). New York:Academic Press.}
\author{Daniela Recchia, Thomas Ostermann.}
\seealso{\link[effsize]{cohen.d},\link[stats]{cor}}
\examples{
cordata("Before","After",data=language_test)}
