#' normal model (t test) using raw vector
#' lower/upper bounds for univariate rv with unknown var
#' @param x random variable
#' @param alpha significance level
#' @return vector of lower/upper bounds of realizations of rv
#' @family models
#' @export
model.normal.bounds <- function(x, alpha=0.05) {
  n <- length(x); xbar <- mean(x); s <- sd(x)  
  t.stat <- qt(1-alpha/2, df=n-1)
  bound <- t.stat*(s/sqrt(n))
  c(lower = xbar - bound, upper = xbar + bound)
}

#' normal model (t test) using summary statistics
#' lower/upper bounds for univariate rv with unknown var 
#' @param xbar sample mean
#' @param sigma2 sample variance
#' @param n number of observation
#' @param alpha significance level
#' @return vector of lower/upper bounds of realizations of rv
#' @family models
#' @export
model.normal.bounds.ss <- function(xbar, sigma, n, alpha=0.05) {
  t.stat <- qt(1-alpha/2, df=n-1)
  bound <- t.stat*(sigma/sqrt(n))
  c(xbar - bound, xbar + bound)
}

#' binomial model (proportion) lower/upper bounds for univariate rv
#' @param p observed or estimated proportion
#' @param n number of observations
#' @param k number of successes
#' @param alpha signifiance level
#' @param type method of approximation
#' @return bounds
#' @family models
#' @export
model.binomial.bounds <- function(p=NULL, k=NULL, n=NULL, alpha=0.05, type = c("normal", "wilson")) {  
  type <- match.arg(type)
  z <- qnorm(1-alpha/2)
  ## Input is data or summary statistics?
  if (!is.null(p) && !is.null(n)) {
    stopifnot(length(p) == 1, length(n) == 1)
    phat <- p
  }
  else if (!is.null(k)) {
    stopifnot(length(k) == 1)
    phat <- k / n
  }
  ## Approximate CI
  if (type == "normal") {
    bound <- z*sqrt((1/n)*phat*(1-phat))
    return(c(phat - bound, phat + bound))
  }
  if (type == "wilson") {
    bound <- z*sqrt( (1/n)*phat*(1-phat) + z^2/(4*n^2) )
    return(c(1/(1+(z^2/n)) * (phat + (1/(2*n))*z^2 - bound)
           , 1/(1+(z^2/n)) * (phat + (1/(2*n))*z^2 + bound)))    
  }
}

#' fit basic structural model
#' @inheritParams StructTS
#' @param x univariate time-series
#' @param frequency frequency of time-series (12 = month, 52 = weekly)
#' @param plot plot components
#' @family models
#' @export
fit_bsm <- function(x, frequency, type = "BSM", plot = FALSE, ...) {
  ts <- ts(x, frequency = frequency)
  struct <- StructTS(ts, type = type, ...)
  smooth <- tsSmooth(struct)
  if (plot) plot(x.smooth)
  ##rowSums(x.smooth)
  rowSums(x.struct$fitted)
}
