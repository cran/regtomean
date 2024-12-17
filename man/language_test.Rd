\name{language_test}
\alias{language_test}
\docType{data}
\title{Language Test in High School}
\description{
A dataset with scores from 8 students who failed a high school test and could not get their diploma. They repeated the exam and got new scores.}
\usage{data("language_test")}
\format{
  A data frame with 8 observations on the following 9 variables.
  \describe{
    \item{\code{Student}}{a numeric vector}
    \item{\code{Before}}{a numeric vector}
    \item{\code{After}}{a numeric vector}
    \item{\samp{Total N}}{a numeric vector}
    \item{\code{Cross}}{a numeric vector}
    \item{\samp{Pre-treatment Mean}}{a numeric vector}
    \item{\samp{Pre-treatment Std}}{a numeric vector}
    \item{\samp{Post-treatment Mean}}{a numeric vector}
    \item{\samp{Post-treatment Std}}{a numeric vector}
  }
}

\source{McClave, J.T; Dietrich, F.H.:"Statistics";New York, Dellen Publishing; 1988.}
\examples{
data(language_test)
## maybe str(language_test) ; plot(language_test) ...
}
\keyword{datasets}
\author{Daniela Recchia, Thomas Ostermann.}
