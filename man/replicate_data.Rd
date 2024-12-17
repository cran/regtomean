\name{replicate_data}
\alias{replicate_data}
\title{Replicates before and after values 100 times.}
\description{
  This function replicates 100 times the "before" and "after" values, given a start and end reference for the population mean (\code{mu}).
}
\usage{
replicate_data(start, end, Before, After, data)
}
\arguments{
  \item{start}{
    A numeric value specifying the start value for \code{mu}.
  }
  \item{end}{
    A numeric value specifying the end value for \code{mu}.
  }
  \item{Before}{
    A numeric vector giving the data values for the first ("before") measurement.
  }
  \item{After}{
    A numeric vector giving the data values for the second ("after") measurement.
  }
  \item{data}{
    An optional data frame containing the \code{Before} and \code{After} variables. If not provided, the \code{Before} and \code{After} vectors must be supplied directly.
  }
}
\details{
  To overcome the limitations of Mee and Chua's test regarding the population mean (\code{mu}),
  this function performs a replication of the data over a specified range of values.
  
  The replicated data is used for systematically estimating the unknown population mean (\code{mu}).
  Further analyses are based on this new dataset.
}
\value{
  A data frame containing the replicated dataset, which includes the columns \code{mu}, \code{before}, and \code{after}.
}
\references{
  Ostermann, T., Willich, Stefan N., & Luedtke, Rainer. (2008). Regression toward the mean - a detection method for unknown population mean based on Mee and Chua's algorithm. BMC Medical Research Methodology.

  Galton, F. (1886). Regression towards mediocrity in hereditary stature. Journal of the Anthropological Institute (15: 246-263).
}
\author{
  Daniela Recchia, Thomas Ostermann.
}
\seealso{
  \link[base]{rep}
}
\examples{
# Example usage of replicate_data
replicate_data(0, 100, "Before", "After", data = language_test)
}