
# Assets--------------------------------------------------

#' Load Asset Data.
#'
#' This function reads in asset data stored in .csv format and returns a 
#' time-series object of the asset data.
#' 
#' @param data_path The path to the directory containing the .csv files.
#' @param assets A vector of asset names to be loaded.
#' @param price_col The name of the price column to be selected (e.g. Open, Close, Low, High).
#' @note
#' The `Date` column in the files should be of the format "%m/%d/%y", that is 01/14/13 with
#' 01 implying the month, 14 the date and 13 the year
#'
#' @return An xts object with asset data.
#' @export
#'
#' @examples
#' \dontrun{
#' asset_data <- asset_loader("path/to/data/folder", c("AAPL", "MSFT"), "Close")
#' }
#'
#' @note
#' The data to be loaded must be in .csv type and also must have the Date, Open, 
#' Low, High and Close Prices of the assest or assests to be loaded.
#'
#' @importFrom dplyr select 
#' @importFrom xts as.xts
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom zoo na.locf
#' @importFrom stats setNames
#' @export
asset_loader <- function(data_path, assets, price_col) {
  l <- file.path(data_path, paste0(assets, ".csv"))
  
  for (i in 1:length(l))
  {0
    temp <- utils::read.csv(l[i])
    Date <- temp$Date
    temp <- dplyr::select(temp, Date, price_col)
    temp$Date <- zoo::as.Date(temp$Date, "%m/%d/%y")
    
    if (i == 1) {
      dfm <- temp |> setNames(c('Date', assets[i]))
    } else {
      dfm <- merge(dfm, temp, "Date", all.x = T, all.y = T) |> setNames(c('Date', assets[1:i]))
    }
  }
  colnames(dfm) <- c("Date", assets)
  rownames(dfm) <- dfm$Date
  dfm <- dplyr::select(dfm, !c(Date)) |>
    zoo::na.locf() |>
    xts::as.xts()
  
  return(dfm)
}

# Returns -----------------------------------------------------------------

#' Compute Weekly Returns of a Vector.
#' 
#' This function takes a numeric vector of asset returns and computes weekly returns.
#' 
#' @param vec a numeric vector of asset returns.
#'
#' @note The input data must be an xts object with dates as rownames.
#' 
#' @return A numeric vector of weekly returns.
#' 
#' @examples
#' \dontrun{
#' # Compute weekly returns of an asset vector
#' asset_returns_xts <- xts(c(0.05, -0.03, 0.02, -0.01, 0.04, -0.02, 0.01),
#'                          order.by = as.Date(c("2023-05-01", "2023-05-02", "2023-05-03",
#'                                              "2023-05-04", "2023-05-05", "2023-05-06",
#'                                              "2023-05-07")))
#' weekly_return(asset_returns_xts)
#' }
#' 
#' @seealso 
#' \code{\link{monthly_return}}, \code{\link{annual_return}}
#'
#' @importFrom quantmod weeklyReturn
#' @importFrom xts is.xts
#'
#' @export
weekly_return <- function(vec){
  if (!xts::is.xts(vec)) {
    stop("Input data must be an xts object with dates as rownames.")
  }
  temp_vec = quantmod::weeklyReturn(vec) |> as.vector()
  return(temp_vec)
}


#' Compute Monthly Returns of a Vector.
#' 
#' This function takes a numeric vector of asset returns and computes monthly returns.
#' 
#' @param vec a numeric vector of asset returns.
#'
#' @note The input data must be an xts object with dates as rownames.
#' 
#' @return A numeric vector of monthly returns.
#' 
#' @examples
#' \dontrun{
#' # Compute monthly returns of an asset vector
#' asset_returns_xts <- xts(c(0.05, -0.03, 0.02, -0.01, 0.04, -0.02, 0.01),
#'                          order.by = as.Date(c("2023-05-01", "2023-05-02", "2023-05-03",
#'                                              "2023-05-04", "2023-05-05", "2023-05-06",
#'                                              "2023-05-07")))
#' monthly_return(asset_returns_xts)
#' }
#' 
#' @seealso 
#' \code{\link{weekly_return}}, \code{\link{annual_return}}
#'
#' @importFrom quantmod monthlyReturn
#' @importFrom xts is.xts
#'
#' @export
monthly_return <- function(vec) {
  if (!xts::is.xts(vec)) {
    stop("Input data must be an xts object with dates as rownames.")
  }
  temp_vec = quantmod::monthlyReturn(vec) |> as.vector()
  return(temp_vec)
}


#' Compute Annual Returns of a Vector.
#' 
#' This function takes a vector of asset returns and computes annual returns.
#' 
#' @param vec a numeric vector of asset returns as an xts object with dates as rownames.
#' 
#' @return A numeric vector of annual returns.
#' 
#' @examples
#' \dontrun{
#' # Compute annual returns of an asset vector
#' asset_returns_xts <- xts(c(0.05, -0.03, 0.02, -0.01, 0.04, -0.02, 0.01),
#' order.by = as.Date(c("2023-05-01", "2023-05-02", "2023-05-03",
#' "2023-05-04", "2023-05-05", "2023-05-06",
#' "2023-05-07")))
#' annual_return(asset_returns_xts)
#' }
#' 
#' @seealso 
#' \code{\link{weekly_return}}, \code{\link{monthly_return}}
#'
#' @importFrom quantmod yearlyReturn
#'
#' @export
annual_return <- function(vec) {
  if (!xts::is.xts(vec)) {
    stop("Input data must be an xts object with dates as rownames.")
  }
  temp_vec = quantmod::yearlyReturn(vec) |> as.vector()
  return(temp_vec)
}

# cumulative returns ------------------------------------------------------

#' Compute Cumulative Returns of a Vector.
#' 
#' This function takes a vector of asset returns and computes the cumulative wealth 
#' generated over time, assuming that the initial wealth was \code{initial_eq}.
#' 
#' @param df_ret an xts object of asset returns, with dates as rownames.
#' @param initial_eq a numeric value representing the initial wealth.
#' 
#' @return An xts object of wealth generated over time.
#' 
#' @examples
#' \dontrun{
#' # Compute cumulative returns of an asset vector
#' library(quantmod)
#' asset_returns_xts <- xts(c(0.05, -0.03, 0.02, -0.01, 0.04, -0.02, 0.01),
#'                          order.by = as.Date(c("2023-05-01", "2023-05-02", "2023-05-03",
#'                                                "2023-05-04", "2023-05-05", "2023-05-06",
#'                                                "2023-05-07")))
#' data.cumret(asset_returns_xts, initial_eq = 100)
#' }
#' 
#' @seealso 
#' \code{\link{weekly_return}}, \code{\link{monthly_return}}, \code{\link{annual_return}}
#'
#' @importFrom xts reclass is.xts
#' @importFrom stats na.omit
#'
#' @export
data.cumret <- function(df_ret, initial_eq) {
  if (!xts::is.xts(df_ret)) {
    stop("Input data must be an xts object with dates as rownames.")
  }
  
  CumRet <- cumprod(stats::na.omit(df_ret) + 1) - 1
  Wealth <- initial_eq + (initial_eq * CumRet)
  Wealth <- reclass(Wealth, df_ret)
  
  return(Wealth)
}

# Fitting -----------------------------------------------------------------


# Functions for fitting distributions: each function returns: parameters, aic, bic

#' Fit Normal Distribution to a Vector/stock prices.
#'
#' This function takes a numeric vector and fits a normal distribution to it using the
#' fitdist function from the fitdistrplus package. It returns a list with the mean
#' and standard deviation parameters of the fitted normal distribution, as well as
#' the AIC and BIC values of the fitted distribution.
#'
#' @param vec a numeric vector to be fitted with a normal distribution.
#'
#' @return A list with the following components:
#' \describe{
#' \item{par}{a numeric vector with the estimated mean and standard deviation
#' parameters of the fitted normal distribution.}
#' \item{aic}{a numeric value representing the Akaike information criterion (AIC)
#' of the fitted distribution.}
#' \item{bic}{a numeric value representing the Bayesian information criterion (BIC)
#' of the fitted distribution.}
#' }
#'
#' @examples
#' \dontrun{
#' # Fit a normal distribution to a vector of returns
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' norm_fit(returns)
#' }
#'
#' @seealso
#' \code{\link{t_fit}}, \code{\link{cauchy_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}},
#' \code{\link{nig_fit}}, \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}},
#' \code{\link{skew.ged_fit}}
#'
#' @importFrom fitdistrplus fitdist
#'
#' @export
norm_fit <- function(vec) {
  suppressWarnings({
  vec <- as.vector(vec)
  temp_norm <- fitdistrplus::fitdist(vec, distr = 'norm')
  
  return(
    list(par = c(temp_norm$estimate["mean"], temp_norm$estimate["sd"]),
         aic = temp_norm$aic,
         bic = temp_norm$bic
    ))
  })
}




#' Fit Student's t Distribution to a vector of returns/stock prices.
#'
#' This function fits the Student's t distribution to a given data vector using the `fit.tuv` function
#' from the `ghyp` package. It returns the estimated parameters along with the AIC and BIC 
#' values for the fitted distribution.
#'
#' @param vec a numeric vector containing the data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#'  \item{par}{a numeric vector of length 5 containing the estimated values for the parameters of the 
#'  fitted distribution: lambda (location), alpha (scale), mu (degrees of freedom), sigma (standard deviation),
#'  and gamma (skewness).}
#'  \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#'  \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#' 
#' @importFrom ghyp fit.tuv
#' 
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' t_fit(returns)
#' }
#' 
#' @seealso
#' \code{\link{t_fit}}, \code{\link{cauchy_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}},
#' \code{\link{nig_fit}}, \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}},
#' \code{\link{skew.ged_fit}}
#' 
#' @export
t_fit <- function(vec) {
  suppressWarnings({
    t.fit <- ghyp::fit.tuv(vec, silent = T, symmetric = F, nu = 12)
    samp = length(vec)
    npar = 4
    
    return(
      list(
        par = c(t.fit@lambda, t.fit@alpha.bar, t.fit@mu, as.numeric(t.fit@sigma), t.fit@gamma),
        aic = t.fit@aic,
        bic = (-2 * t.fit@llh) + (npar*log(samp))
        
      )
    )
  })
}


#' Fit Cauchy Distribution to a vector of returns/stock prices.
#'
#' This function fits the Cauchy distribution to a given data vector using the fitdist function
#' from the fitdistrplus package. It returns the estimated parameters along with the AIC and BIC
#' values for the fitted distribution.
#'
#' @param vec a numeric vector containing the data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#' \item{par}{a numeric vector of length 2 containing the estimated values for the parameters of the
#' fitted distribution: lambda (location) and alpha (scale).}
#' \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#' \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#'
#' @importFrom fitdistrplus fitdist
#'
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' cauchy_fit(returns)
#' }
#'
#' @seealso
#' \code{\link{t_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}},
#' \code{\link{nig_fit}}, \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}},
#' \code{\link{skew.ged_fit}}
#'
#' @export
cauchy_fit <- function(vec) {
  suppressWarnings({
  vec <- as.vector(vec)
  temp_cauchy <- fitdistrplus::fitdist(vec, distr = 'cauchy')
  return(
    list(par = c(temp_cauchy$estimate["location"], temp_cauchy$estimate["scale"]),
         aic = temp_cauchy$aic,
         bic = temp_cauchy$bic
    ))
  })
}


#' Fit Generalized Hyperbolic Distribution to a vector of returns/stock prices.
#'
#' This function fits the Generalized Hyperbolic (GH) distribution to a given data vector using the 
#' `fit.ghypuv` function from the `ghyp` package. It returns the estimated parameters along with 
#' the AIC and BIC values for the fitted distribution.
#'
#' @param vec a numeric vector containing the data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#'  \item{par}{a numeric vector of length 5 containing the estimated values for the parameters of the 
#'  fitted distribution: lambda (location), alpha (scale), mu (degrees of freedom), sigma (standard deviation),
#'  and gamma (skewness).}
#'  \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#'  \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#' 
#' @importFrom ghyp fit.ghypuv
#' 
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' ghd_fit(returns)
#' }
#' 
#' @seealso
#' \code{\link{t_fit}}, \code{\link{cauchy_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}},
#' \code{\link{nig_fit}}, \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}},
#' \code{\link{skew.ged_fit}}
#' 
#' @export
ghd_fit <- function(vec) {
  suppressWarnings({
    ghd.fit <- ghyp::fit.ghypuv(vec, silent = T, symmetric = F)
    samp = length(vec)
    npar = 5
    
    return(
      list(
        par = c(ghd.fit@lambda, ghd.fit@alpha.bar, ghd.fit@mu, as.numeric(ghd.fit@sigma),
                ghd.fit@gamma),
        aic = ghd.fit@aic,
        bic = (-2 * ghd.fit@llh) + (npar*log(samp))
        
      )
    )
  })
}


#' Fit Hyperbolic distribution to return/stock prices.
#'
#' This function fits the Hyperbolic distribution to a given data vector using the `fit.hypuv` 
#' function from the `ghyp` package. It returns the estimated parameters along with the AIC and 
#' BIC values for the fitted distribution.
#'
#' @param vec a numeric vector containing the data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#'  \item{par}{a numeric vector of length 4 containing the estimated values for the parameters of the 
#'  fitted distribution: alpha (scale), mu (location), sigma (standard deviation), and gamma (skewness).}
#'  \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#'  \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#'
#' @importFrom ghyp fit.hypuv
#'
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' hd_fit(returns)
#' }
#'
#' @seealso
#' \code{\link{hd_fit}}, \code{\link{sym.ghd_fit}}, \code{\link{ghd_fit}}, \code{\link{cauchy_fit}},
#' \code{\link{t_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}},
#' \code{\link{nig_fit}}, \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}},
#' \code{\link{skew.ged_fit}}
#'
#' @export
hd_fit <- function(vec) {
  suppressWarnings({
    hd.fit <- ghyp::fit.hypuv(vec, silent = T, symmetric = F)
    samp = length(vec)
    npar = 4
    
    return(
      list(
        par = c(hd.fit@alpha.bar, hd.fit@mu, as.numeric(hd.fit@sigma),
                hd.fit@gamma),
        aic = hd.fit@aic,
        bic = (-2 * hd.fit@llh) + (npar*log(samp))
        
      )
    )
    })
}
    

#' Fit Symmetric Generalized Hyperbolic Distribution to returns/stock prices.
#'
#' This function fits the Symmetric Generalized Hyperbolic (sGH) distribution to a given data vector using 
#' the `fit.ghypuv` function from the `ghyp` package. It returns the estimated parameters along with 
#' the AIC and BIC values for the fitted distribution.
#'
#' @param vec a numeric vector containing the data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#'  \item{par}{a numeric vector of length 5 containing the estimated values for the parameters of the 
#'  fitted distribution: lambda (location), alpha (scale), mu (degrees of freedom), sigma (standard deviation),
#'  and gamma (skewness).}
#'  \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#'  \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#' 
#' @importFrom ghyp fit.ghypuv
#' 
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' sym.ghd_fit(returns)
#' }
#' 
#' @seealso
#' \code{\link{t_fit}}, \code{\link{cauchy_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}},
#' \code{\link{nig_fit}}, \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}},
#' \code{\link{skew.ged_fit}}
#' 
#' @export
sym.ghd_fit <- function(vec) {
  suppressWarnings({
    ghd.fit <- ghyp::fit.ghypuv(vec, silent = T, symmetric = T)
    samp = length(vec)
    npar = 4
    
    return(
      list(
        par = c(ghd.fit@lambda, ghd.fit@alpha.bar, ghd.fit@mu, as.numeric(ghd.fit@sigma),
                ghd.fit@gamma),
        aic = ghd.fit@aic,
        bic = (-2 * ghd.fit@llh) + (npar*log(samp))
        
      )
    )
  })
}

#' Fit a Symmetric Hyperbolic Distribution to a vector of return/stock prices.
#'
#' This function fits a Symmetric Hyperbolic distribution to a  data vector using the 
#' `fit.hypuv` function from the `ghyp` package. It returns the estimated parameters along with 
#' the AIC and BIC values for the fitted distribution.
#'
#' @param vec a numeric vector containing the symmetric data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#'  \item{par}{a numeric vector of length 4 containing the estimated values for the parameters of the 
#'  fitted distribution: alpha (scale), mu (degrees of freedom), sigma (standard deviation), and gamma (skewness).}
#'  \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#'  \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#' 
#' @importFrom ghyp fit.hypuv
#' 
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' sym.hd_fit(returns)
#' }
#' 
#' @seealso
#' \code{\link{t_fit}}, \code{\link{cauchy_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}}, \code{\link{nig_fit}},
#' \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}}, \code{\link{skew.ged_fit}}
#' 
#' @export
sym.hd_fit <- function(vec) {
  suppressWarnings({
    hd.fit <- ghyp::fit.hypuv(vec, silent = T, symmetric = T)
    samp = length(vec)
    npar = 3
    
    return(
      list(
        par = c(hd.fit@alpha.bar, hd.fit@mu, as.numeric(hd.fit@sigma),
                hd.fit@gamma),
        aic = hd.fit@aic,
        bic = (-2 * hd.fit@llh) + (npar*log(samp))
      )
    )
  })
}

#' Fit Variance Gamma Distribution to a vector of return/stock prices.
#'
#' This function fits the Variance Gamma (VG) distribution to a given data vector using the 
#' `fit.VGuv` function from the `ghyp` package. It returns the estimated parameters along with 
#' the AIC and BIC values for the fitted distribution.
#'
#' @param vec a numeric vector containing the data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#'  \item{par}{a numeric vector of length 4 containing the estimated values for the parameters of the 
#'  fitted distribution: lambda (location), mu (scale), sigma (shape), and gamma (skewness).}
#'  \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#'  \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#'
#' @importFrom ghyp fit.VGuv
#'
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' vg_fit(returns)
#' }
#'
#' @seealso
#' \code{\link{t_fit}}, \code{\link{cauchy_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}},
#' \code{\link{nig_fit}}, \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}},
#' \code{\link{skew.ged_fit}}
#'
#' @export
vg_fit <- function(vec) {
  suppressWarnings({
    suppressMessages({
      vgdist.fit <- ghyp::fit.VGuv(vec, silent = T, symmetric = F)
      samp = length(vec)
      npar = 4
      
      return(
        list(
          par = c(vgdist.fit@lambda, vgdist.fit@mu, as.numeric(vgdist.fit@sigma),
                  vgdist.fit@gamma),
          aic = vgdist.fit@aic,
          bic = (-2 * vgdist.fit@llh) + (npar*log(samp))
        )
      )
    })
  })
}


#' Fit Symmetric Variance Gamma Distribution to a vector of returns/stock prices.
#'
#' This function fits the Symmetric Variance Gamma (sVG) distribution to a given data vector using the 
#' `fit.VGuv` function from the `ghyp` package. It returns the estimated parameters along with 
#' the AIC and BIC values for the fitted distribution.
#'
#' @param vec a numeric vector containing the data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#'  \item{par}{a numeric vector of length 4 containing the estimated values for the parameters of the 
#'  fitted distribution: lambda (scale), mu (location), sigma (volatility), and gamma (skewness).}
#'  \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#'  \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#' 
#' @importFrom ghyp fit.VGuv
#' 
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' sym.vg_fit(returns)
#' }
#' 
#' @seealso
#' \code{\link{t_fit}}, \code{\link{cauchy_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{nig_fit}}, 
#' \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}}, \code{\link{skew.ged_fit}}
#' 
#' @export
sym.vg_fit <- function(vec) {
  suppressWarnings({
    suppressMessages({
      vgdist.fit <- ghyp::fit.VGuv(vec, silent = T, symmetric = T)
      samp = length(vec)
      npar = 4
      
      return(
        list(
          par = c(vgdist.fit@lambda, vgdist.fit@mu, as.numeric(vgdist.fit@sigma),
                  vgdist.fit@gamma),
          aic = vgdist.fit@aic,
          bic = (-2 * vgdist.fit@llh) + (npar*log(samp))
          
        )
      )
    })
  })
}


#' Fit Normal Inverse Gaussian (NIG) Distribution to a vector of returns/stock prices.
#'
#' This function fits the Normal Inverse Gaussian (NIG) Distribution to a given data vector using the `nig_fit` function from
#' the `fBasics` package. It returns the estimated parameters along with the AIC and BIC values for the fitted
#' distribution.
#'
#'
#' @param vec A numeric vector of data.
#' @return A list with the following elements:
#' \describe{
#'   \item{params}{The estimated parameters of the NIG distribution: location, scale, skewness, and shape.}
#'   \item{aic}{The Akaike Information Criterion (AIC) for the NIG distribution fit.}
#'   \item{bic}{The Bayesian Information Criterion (BIC) for the NIG distribution fit.}
#' }
#' @examples
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' nig_fit(returns)
#' 
#' @importFrom fBasics nigFit
#' 
#' @export
nig_fit <- function(vec) {
  suppressWarnings({
    temp_nig <- fBasics::nigFit(vec, trace = F, doplot = F)
    deviance = 2 * (temp_nig@fit$objective)
    npar = 4
    samp = length(vec)
    
    return(list(
      params = temp_nig@fit$par,
      aic = deviance + (2 * npar),
      bic = deviance + npar * log(samp)
    ))
  })
}


#' Fit Generalized Error Distribution to a vector of returns/stock prices.
#'
#' This function fits the Generalized Error Distribution (GED) to a given data vector using the `ged_fit` function from
#' the `fGarch` package. It returns the estimated parameters along with the AIC and BIC values for the fitted
#' distribution.
#'
#' @param vec A numeric vector of data.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{params}{A numeric vector of length 3 containing the fitted GED parameters: shape, scale, and location.}
#'   \item{aic}{The Akaike Information Criterion (AIC) for the fitted model.}
#'   \item{bic}{The Bayesian Information Criterion (BIC) for the fitted model.}
#' }
#'
#'@importFrom fGarch gedFit
#' @examples
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' ged_fit(returns)
#'
#' @export
ged_fit <- function(vec) {
  suppressWarnings({
    temp_ged <- fGarch::gedFit(vec)
    deviance = 2 * (temp_ged$objective)
    npar = 3
    samp = length(vec)
    
    return(list(
      params = temp_ged$par,
      aic = deviance + (2 * npar),
      bic = deviance + npar * log(samp)
    ))
  })
}


#' Fit Skewed Student-t Distribution to a vector of returns/stock prices.
#'
#' This function fits the Skewed Student-t Distribution to a given data vector using the `skew.t_fit` function from
#' the `fGarch` package. It returns the estimated parameters along with the AIC and BIC values for the fitted
#' distribution.
#'
#' @param vec A numeric vector of data.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{params}{A numeric vector of length 4 containing the fitted Skewed Student-t parameters: degrees of freedom, skewness, scale, and location.}
#'   \item{aic}{The Akaike Information Criterion (AIC) for the fitted model.}
#'   \item{bic}{The Bayesian Information Criterion (BIC) for the fitted model.}
#' }
#'
#' @importFrom fGarch sstdFit
#' @examples
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' skew.t_fit(returns)
#'
#' @export
skew.t_fit <- function(vec) {
  suppressWarnings({
    temp_skew.t <- fGarch::sstdFit(vec)
    deviance = 2 * (temp_skew.t$minimum)
    npar = 4
    samp = length(vec)
    
    return(list(
      params = temp_skew.t$estimate,
      aic = deviance + (2 * npar),
      bic = deviance + npar * log(samp)
    ))
  })
}


#' Fit Skew Normal Distribution to a vector of returns/stock prices.
#'
#' This function fits the Skew Normal distribution to a given data vector using the `snormFit` function from
#' the `fGarch` package. It returns the estimated parameters along with the AIC and BIC values for the fitted
#' distribution.
#'
#' @param vec a numeric vector containing the data to be fitted.
#'
#' @return a list containing the following elements:
#' \describe{
#'  \item{params}{a numeric vector of length 3 containing the estimated values for the parameters of the
#'  fitted distribution: location (mu), scale (sigma), and skewness (alpha).}
#'  \item{aic}{the Akaike information criterion (AIC) value for the fitted distribution.}
#'  \item{bic}{the Bayesian information criterion (BIC) value for the fitted distribution.}
#' }
#'
#' @importFrom fGarch snormFit
#'
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' skew.normal_fit(returns)
#' }
#'
#' @seealso
#' \code{\link{t_fit}}, \code{\link{cauchy_fit}}, \code{\link{ghd_fit}}, \code{\link{hd_fit}},
#' \code{\link{sym.ghd_fit}}, \code{\link{sym.hd_fit}}, \code{\link{vg_fit}}, \code{\link{sym.vg_fit}},
#' \code{\link{nig_fit}}, \code{\link{ged_fit}}, \code{\link{skew.t_fit}}, \code{\link{skew.normal_fit}},
#' \code{\link{skew.ged_fit}}
#'
#' @export
skew.normal_fit <- function(vec) {
  suppressWarnings({
    temp_skew.n <- fGarch::snormFit(vec)
    deviance = 2 * (temp_skew.n$objective)
    npar = 4
    samp = length(vec)
    
    return(list(
      params = temp_skew.n$par,
      aic = deviance + (2 * npar),
      bic = deviance + npar * log(samp)
    ))
  })
}


#' Fit Skewed Generalized Error Distribution to a vector of returns/stock prices.
#'
#' This function fits the Skewed Generalized Error Distribution to a given data vector using the `skew.ged_fit` function from
#' the `fGarch` package. It returns the estimated parameters along with the AIC and BIC values for the fitted
#' distribution.
#' 
#' @param vec A numeric vector of data.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{params}{A numeric vector of length 4 containing the fitted SGED parameters: shape, scale, location, and skewness.}
#'   \item{aic}{The Akaike Information Criterion (AIC) for the fitted model.}
#'   \item{bic}{The Bayesian Information Criterion (BIC) for the fitted model.}
#' }
#'
#' @importFrom fGarch sgedFit
#' 
#' @examples
#' \dontrun{
#' stock_prices <- c(10, 11, 12, 13, 14)
#' returns <- diff(log(stock_prices))
#' skew.ged_fit(returns)
#' }
#' 
#' @export
skew.ged_fit <- function(vec) {
  suppressWarnings({
    temp_skew.ged <- fGarch::sgedFit(vec)
    deviance = 2 * (temp_skew.ged$objective)
    npar = 4
    samp = length(vec)
    
    return(list(
      params = temp_skew.ged$par,
      aic = deviance + (2 * npar),
      bic = deviance + npar * log(samp)
    ))
  })
}


# Multiple Function -------------------------------------------------------

#' Fits Multiple Probability Distributions to several assets/stock prices.
#'
#' This function fits multiple probability distributions to a dataframe and calculates the 
#' Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC) for each distribution and
#' then returns a data frame of the AIC values for each asset where the column names are the names of the fitted
#' @details
#' distributions. Note that the available distributions are
#' norm_fit - Normal distribution
#' t_fit - Student's t-distribution
#' cauchy_fit - Cauchy distribution
#' ghd_fit - Generalized hyperbolic distribution
#' hd_fit - Hyperbolic distribution
#' sym.ghd_fit - Symmetric generalized hyperbolic distribution
#' sym.hd_fit - Symmetric hyperbolic distribution
#' vg_fit - Variance-gamma distribution
#' sym.vg_fit - Symmetric variance-gamma distribution
#' nig_fit - Normal-inverse Gaussian distribution
#' ged_fit - Generalized error distribution
#' skew.t_fit - Skew Student's t-distribution
#' skew.normal_fit - Skew normal distribution
#' skew.ged_fit - Skew generalized error distribution
#' Also note that the distribution to be fitted from the above list must include the '_fit'.
#' The function can also fit one distribution to one asset.
#' 
#' @param dist_names a character vector of distribution names to be fitted.
#' @param dataframe a dataframe containing the data to be fitted.
#' @return A list of distributions and their corresponding AIC and BIC values.
#' @examples
#' \dontrun{
#' data = asset_loader("path/to/data/folder", c("asset1", "asset2"), "Close")
#' fit_multiple_dist(c("norm_fit", "cauchy_fit"), data)
#' }
#' 
#' @seealso \code{\link{asset_loader}}
#' @importFrom fitdistrplus fitdist 
#' @importFrom ghyp fit.tuv fit.ghypuv fit.hypuv fit.VGuv
#' @importFrom fGarch gedFit sstdFit snormFit sgedFit
#' @importFrom fBasics nigFit
#' 
#' @export
fit_multiple_dist <- function(dist_names, dataframe) {
  
  dist <- c('norm_fit', 't_fit', 'cauchy_fit', 'ghd_fit', 'hd_fit',
                  'sym.ghd_fit', 'sym.hd_fit', 'vg_fit', 'sym.vg_fit',
                  'nig_fit', 'ged_fit', 'skew.t_fit', 'skew.normal_fit',
                  'skew.ged_fit')
  if(!all(dist_names %in% dist)) {
    not_found <- dist_names[!dist_names %in% dist]
    stop(paste0("\nDistribution ", not_found, " not found. Check help for available distributions"))
  }

  norm_fit <- function(vec) {
    vec <- as.vector(vec)
    temp_norm <- fitdistrplus::fitdist(vec, distr = 'norm')
    
    return(
      list(par = c(temp_norm$estimate["mean"], temp_norm$estimate["sd"]),
           aic = temp_norm$aic,
           bic = temp_norm$bic
      ))
  }
  
  t_fit <- function(vec) {
    suppressWarnings({
      t.fit <- ghyp::fit.tuv(vec, silent = T, symmetric = F, nu = 12)
      samp = length(vec)
      npar = 4
      
      return(
        list(
          par = c(t.fit@lambda, t.fit@alpha.bar, t.fit@mu, as.numeric(t.fit@sigma), t.fit@gamma),
          aic = t.fit@aic,
          bic = (-2 * t.fit@llh) + (npar*log(samp))
          
        )
      )
    })
  }
  
  cauchy_fit <- function(vec) {
    vec <- as.vector(vec)
    temp_cauchy <- fitdistrplus::fitdist(vec, distr = 'cauchy')
    return(
      list(par = c(temp_cauchy$estimate["location"], temp_cauchy$estimate["scale"]),
           aic = temp_cauchy$aic,
           bic = temp_cauchy$bic
      ))
  }
  
  ghd_fit <- function(vec) {
    suppressWarnings({
      ghd.fit <- ghyp::fit.ghypuv(vec, silent = T, symmetric = F)
      samp = length(vec)
      npar = 5
      
      return(
        list(
          par = c(ghd.fit@lambda, ghd.fit@alpha.bar, ghd.fit@mu, as.numeric(ghd.fit@sigma),
                  ghd.fit@gamma),
          aic = ghd.fit@aic,
          bic = (-2 * ghd.fit@llh) + (npar*log(samp))
          
        )
      )
    })
  }
  
  hd_fit <- function(vec) {
    suppressWarnings({
      hd.fit <- ghyp::fit.hypuv(vec, silent = T, symmetric = F)
      samp = length(vec)
      npar = 4
      
      return(
        list(
          par = c(hd.fit@alpha.bar, hd.fit@mu, as.numeric(hd.fit@sigma),
                  hd.fit@gamma),
          aic = hd.fit@aic,
          bic = (-2 * hd.fit@llh) + (npar*log(samp))
          
        )
      )
    })
  }
  
  sym.ghd_fit <- function(vec) {
    suppressWarnings({
      ghd.fit <- ghyp::fit.ghypuv(vec, silent = T, symmetric = T)
      samp = length(vec)
      npar = 4
      
      return(
        list(
          par = c(ghd.fit@lambda, ghd.fit@alpha.bar, ghd.fit@mu, as.numeric(ghd.fit@sigma),
                  ghd.fit@gamma),
          aic = ghd.fit@aic,
          bic = (-2 * ghd.fit@llh) + (npar*log(samp))
          
        )
      )
    })
  }
  
  sym.hd_fit <- function(vec) {
    suppressWarnings({
      hd.fit <- ghyp::fit.hypuv(vec, silent = T, symmetric = T)
      samp = length(vec)
      npar = 3
      
      return(
        list(
          par = c(hd.fit@alpha.bar, hd.fit@mu, as.numeric(hd.fit@sigma),
                  hd.fit@gamma),
          aic = hd.fit@aic,
          bic = (-2 * hd.fit@llh) + (npar*log(samp))
        )
      )
    })
  }
  
  vg_fit <- function(vec) {
    suppressWarnings({
      suppressMessages({
        vgdist.fit <- ghyp::fit.VGuv(vec, silent = T, symmetric = F)
        samp = length(vec)
        npar = 4
        
        return(
          list(
            par = c(vgdist.fit@lambda, vgdist.fit@mu, as.numeric(vgdist.fit@sigma),
                    vgdist.fit@gamma),
            aic = vgdist.fit@aic,
            bic = (-2 * vgdist.fit@llh) + (npar*log(samp))
          )
        )
      })
    })
  }
  
  sym.vg_fit <- function(vec) {
    suppressWarnings({
      suppressMessages({
        vgdist.fit <- ghyp::fit.VGuv(vec, silent = T, symmetric = T)
        samp = length(vec)
        npar = 4
        
        return(
          list(
            par = c(vgdist.fit@lambda, vgdist.fit@mu, as.numeric(vgdist.fit@sigma),
                    vgdist.fit@gamma),
            aic = vgdist.fit@aic,
            bic = (-2 * vgdist.fit@llh) + (npar*log(samp))
            
          )
        )
      })
    })
  }
  
  nig_fit <- function(vec) {
    suppressWarnings({
      temp_nig <- fBasics::nigFit(vec, trace = F, doplot = F)
      deviance = 2 * (temp_nig@fit$objective)
      npar = 4
      samp = length(vec)
      
      return(list(
        params = temp_nig@fit$par,
        aic = deviance + (2 * npar),
        bic = deviance + npar * log(samp)
      ))
    })
  }
  
  ged_fit <- function(vec) {
    suppressWarnings({
      temp_ged <- fGarch::gedFit(vec)
      deviance = 2 * (temp_ged$objective)
      npar = 3
      samp = length(vec)
      
      return(list(
        params = temp_ged$par,
        aic = deviance + (2 * npar),
        bic = deviance + npar * log(samp)
      ))
    })
  }
  
  skew.t_fit <- function(vec) {
    suppressWarnings({
      temp_skew.t <- fGarch::sstdFit(vec)
      deviance = 2 * (temp_skew.t$minimum)
      npar = 4
      samp = length(vec)
      
      return(list(
        params = temp_skew.t$estimate,
        aic = deviance + (2 * npar),
        bic = deviance + npar * log(samp)
      ))
    })
  }
  
  skew.normal_fit <- function(vec) {
    suppressWarnings({
      temp_skew.n <- fGarch::snormFit(vec)
      deviance = 2 * (temp_skew.n$objective)
      npar = 4
      samp = length(vec)
      
      return(list(
        params = temp_skew.n$par,
        aic = deviance + (2 * npar),
        bic = deviance + npar * log(samp)
      ))
    })
  }
  
  skew.ged_fit <- function(vec) {
    suppressWarnings({
      temp_skew.ged <- fGarch::sgedFit(vec)
      deviance = 2 * (temp_skew.ged$objective)
      npar = 4
      samp = length(vec)
      
      return(list(
        params = temp_skew.ged$par,
        aic = deviance + (2 * npar),
        bic = deviance + npar * log(samp)
      ))
    })
  }
  
  dd <- dataframe
  for (i in seq_along(dist_names))
  {
    message('Fitting distribution: ', dist_names[i])
    dist.fit <- apply(dd, 2, dist_names[i])
    
    if (i == 1)
    {
      aic_df <- dist.fit |>
        lapply(
          FUN = function(x)
            x$aic
        ) |>
        list2DF() |>
        t() |>
        data.frame()
    }
    else
    {
      aic_df <- cbind(
        aic_df,
        dist.fit |>
          lapply(
            FUN = function(x)
              x$aic
          ) |>
          list2DF() |>
          t() |>
          data.frame()
      )
    }
  }
  message("The AIC for the fitted distribution(s) have been stored")
  aic_df <- aic_df |>
    setNames(dist_names)
  return(aic_df)
}

#' Find the best distribution based on AIC values
#'
#' This function takes in a data frame of AIC values for different distributions
#' and a vector of distribution names, and returns a data frame with the best
#' distribution for each row based on the minimum AIC value.
#' #' You can also write the distribution as "norm" or "cauchy" provided they
#' follow the order in the data frame. 
#' 
#'@note
#'This function takes the data frame obtained from `fit_multiple_dist` function
#' @param aic_df A data frame containing AIC values for different distributions
#' @param dist_names A vector of distribution names corresponding to the AIC values
#' @return A data frame with the best distribution for each row based on the minimum AIC value
#' @examples
#' \dontrun{
#' data = asset_loader("path/to/data", c("asset1", "asset2"), "Close")
#' df = fit_multiple_dist(c("norm_fit", "cauchy_fit"), data)
#' best_dist(df, c("norm_fit", "cauchy_fit"))
#' }
#' 
#' @importFrom dplyr mutate
#' 
#' @export
best_dist <- function(aic_df, dist_names){
  message("Comparing the distributions")
  best_aic <- aic_df |>
    apply(MARGIN = 1, FUN = which.min) |>
    as.numeric()
  
  aic_df <- aic_df |>
    dplyr::mutate(best_aic = dist_names[best_aic])
  return(aic_df)
}
