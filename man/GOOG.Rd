\name{GOOG}
\alias{GOOG}
\docType{data}
\title{Alphabet Inc Inc. Stock Prices Dataset}
\description{This dataset contains the daily stock prices of Alphabet Inc. (GOOG) from January 2, 2013 to April 30, 2023. The data includes the open, high, low, and close prices, as well as the volume and adjusted close price. ~~
}
\usage{data("GOOG")}
\format{
  A data frame with 2599 observations on the following 7 variables.
  \describe{
    \item{\code{Open}}{a numeric vector}
    \item{\code{High}}{a numeric vector}
    \item{\code{Low}}{a numeric vector}
    \item{\code{Close}}{a numeric vector}
    \item{\code{Volume}}{a numeric vector}
    \item{\code{Adjusted}}{a numeric vector}
    \item{\code{Date}}{a character vector}
  }
}
\references{Data source: Yahoo Finance}
\examples{
data(GOOG)
str(GOOG) ; plot(GOOG)
}
\keyword{datasets}
