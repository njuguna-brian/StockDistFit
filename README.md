
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StockDistFit

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/njuguna-brian/StockDistFit.svg?branch=main)](https://travis-ci.com/njuguna-brian/StockDistFit)
[![R-CMD-check](https://github.com/njuguna-brian/StockDistFit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/njuguna-brian/StockDistFit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of StockDistFit is to provide functions that help in fitting
probability distributions to financial data, specifically stock returns
and prices. These functions can be used to compare the goodness of fit
of different distributions and choose the most appropriate one, which
can aid in making investment decisions or modeling financial phenomena.
The package also includes function for cumulative wealth generated over
time, given the initial wealth. Overall, StockDistFit aims to simplify
the process of fitting and analyzing probability distributions for
financial data.

## Installation

You can install the development version of StockDistFit from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("njuguna-brian/StockDistFit")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(StockDistFit)
df <- asset_loader("path/to/data/folder", "AAPL", "Close")
df_returns <- weekly_return(df)

# Fit a normal Distribution to the Closing Price
norm_fit(df_returns)
```
