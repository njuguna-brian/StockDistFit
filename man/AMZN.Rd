\name{AMZN}
\alias{AMZN}
\docType{data}
\title{Amazon.com Inc. Stock Prices Dataset}
\description{This dataset contains the daily stock prices of Amazon.com Inc. (AMZN) from January 2, 2013 to April 30, 2023. The data includes the open, high, low, and close prices, as well as the volume and adjusted close price. ~~
}
\usage{data("AMZN")}
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
data(AMZN)
str(AMZN) ; plot(AMZN)
}
\keyword{datasets}
