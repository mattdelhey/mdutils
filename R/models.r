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


model.exp.posterior <- function(xbar, n, alpha, beta, samples = 10000) {
  ## posterior mean: (alpha + n) / (beta + n*xbar)
  ## alpha = mean(historical data)
  ## beta = sd(historical data)
  1/rgamma(samples, alpha + n, beta + n*xbar)
}

model.exp.predictive <- function(alpha, beta, samples = 10000) {
  ## posterior mean: beta / alpha - 1
  ## or lambda / alpha - 1
  alpha.prime <- alpha + 1
  beta.prime <- beta + 1 ## estimated time interval
  VGAM::rlomax(samples, alpha.prime, beta.prime)
}

model.exp.ci.gamma <- function(xbar, n, alpha = 0.95) {
  lower_stat <- qgamma((1-alpha)/2, n, n)
  upper_stat <- qgamma(1-(1-alpha)/2, n, n)
  1/c(lower = upper_stat/xbar, lower_stat/xbar)
}

model.exp.ci.chi <- function(xbar, n, alpha = 0.95) {
  lower <- 2*n*xbar / pchisq(p = (1-alpha)/2, df = 2*n)
  upper <- 2*n*xbar / pchisq(p = 1 - (1-alpha)/2, df = 2*n)
  c(lower = lower, upper = upper)
}

model.exp.ci.approx <- function(xbar, n, alpha = 0.95) {
  lambda.hat <- 1/xbar
  sig <- 1-(1-alpha)/2
  score <- qnorm(sig)
  lower <- lambda.hat * (1 - score/sqrt(n))
  upper <- lambda.hat * (1 + score/sqrt(n))
  c(lower = 1/upper, upper = 1/lower)
}

if (FALSE) {
  model.exp.ci.gamma(0.5, 10)
  model.exp.ci.approx(0.5, 10)
  sd(model.exp.posterior(0.5, 100, 10, 5))
  summary(model.exp.posterior(0.5, 100, 10, 5))
  ci <- model.exp.ci(1/traffic$rpc, traffic$clicks)
}

