\name{AAPL}
\alias{AAPL}
\docType{data}
\title{Apple Inc. stock prices dataset}
\description{This dataset contains the daily stock prices of Apple Inc. (AAPL) from January 2, 2013 to April 30, 2023. The data includes the open, high, low, and close prices, as well as the volume and adjusted close price. ~~
}
\usage{data("AAPL")}
\format{
  A data frame with 2599 observations on the following 7 variables.
  \describe{
    \item{\code{Date}}{a character vector}
    \item{\code{Open}}{a numeric vector}
    \item{\code{High}}{a numeric vector}
    \item{\code{Low}}{a numeric vector}
    \item{\code{Close}}{a numeric vector}
    \item{\code{Volume}}{a numeric vector}
    \item{\code{Adjusted}}{a numeric vector}
  }
}
\references{Data source: Yahoo Finance}
\examples{
data(AAPL)
str(AAPL) ; plot(AAPL)
}
\keyword{datasets}
