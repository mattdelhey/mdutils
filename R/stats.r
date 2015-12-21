#' Sample variance (unbiased)
#' @param x data vector
#' @param x2 data vector squared
#' @family stats
#' @export
sample.var <- function(x, x2) {
  n <- length(x)
  1/(n*(n-1)) * (n*sum(x2) - sum(x)^2)
}

sample.var.ss <- function() {  
}

#' bootstrap a statistic for a vector
#' @param x data vector
#' @param t statistic
#' @param n number of simulations
#' @family stats
#' @export
bstrap <- function(x, t = mean, n = 10000) {
  replicate(n, {
    t(sample(x, length(x), replace = TRUE))
  })
}

#' Summarize a coefficient matrix
#' @param beta matrix of coefficient where rows represent variables and columns represent hyperparameter
#' @family stats
#' @export
summarize.coefficient.matrix <- function(beta) {
  beta.bar <- rowMeans(beta)
  data.frame(
    variable = dimnames(beta)[[1]]
  , beta = sort.abs(beta.bar, decreasing = TRUE)
  , sign = sign(beta.bar))
}

#' Generic dispatch for importance
#' @param x model fit object with correct class
#' @family stats
#' @export
importance <- function(x, ...) {
  UseMethod("importance", x)
}

#' Variable importance for lm fit
#' Importance determined by absolute value of t-statistic
#' @param x lm fit
#' @family stats
#' @export
importance.lm <- function(x, ...) {
  coef <- coef(summary(x))
  tval <- sort.abs(coef[, "t value"])  
  data.frame(
    variable = names(tval)
  , tval = as.numeric(tval)
  )
}

#' Variable importance for glmnet fit
#' Importance determined by absolute value of mean coefficient
#' @param x glmnet fit
#' @family stats
#' @export
importance.glmnet <- function(x, ...) {
  beta <- x$beta
  summarize.coefficient.matrix(beta)
}

#' Variable importance for cv.glmnet fit
#' Importance determined by absolute value of mean coefficient
#' @param x cv.glmnet fit
#' @family stats
#' @export
importance.cv.glmnet <- function(x, ...) {  
  beta <- x$glmnet.fit$beta
  summarize.coefficient.matrix(beta)
}

#' Add bias column to data matrix
#' @param x data matrix
#' @family stats
#' @export
add.bias <- function(x) {
  stopifnot(is.matrix(x) || is.data.frame(x))
  n <- nrow(x)
  res <- cbind(cbind(rep(1, n)), x)
  if (is.data.frame(x)) res <- as.data.frame(res)
  res
}

#' Derivative estimation of 1d function
#' Uses change-in-y / chaing-in-x approximation
#' @param x input values
#' @param y output values
#' @family stats
#' @export
emperical.derivative <- function(x, y) {
  diff(y) / diff(x)
}

#' Derivative estimation of 1d function
#' @param x input values
#' @param y output values
#' @family stats
#' @export
spline.derivative <- function(x, y, deriv = 1, ...) {
  spline <- smooth.spline(x, y, ...)
  out <- predict(spline, deriv = deriv)
  out$fit <- predict(spline)$y
  out
}


#' Sphere data. This is a stronger enforcement than scaling as it forces Cov = I.
#' @param x data matrix to be sphered
#' @family stats
#' @export
sphere <- function(x) {
  x <- as.matrix(x)
  nm <- colnames(x)
  x <- scale(x, center = TRUE, scale = FALSE)
  s <- svd(var(x))
  w <- t(s$v %*% (t(s$u) * (1/sqrt(s$d))))
  res <- as.data.frame(x %*% w)
  names(res) <- nm
  res         
}

#' Rescale a matrix or data frame
#' Standardise each column to have range [0, 1]
#' @param x data frame or matrix
#' @return rescaled x
#' @family stats
#' @export
rescale <- function(x) {
  apply(x, 2, function(z) (z - min(z)) / diff(range(z)))
}

#' Center a numeric vector by subtracting off its mean.
#' @param x numeric vector
#' @family stats
#' @export
center <- function(x) {
  scale(x, center = TRUE, scale = FALSE)
}

#' moving average
#' @param x a univariate or multivariate time series
#' @param n number of observations to include
#' @param sides if sides = 1 the filter coefficients are for past values only; if sides = 2 they are
#'   centred around lag 0. In this case the length of the filter should be odd, but if it is even,
#'   more of the filter is forward in time than backward.
#' @family stats
#' @export
ma <- function(x, n, sides = 1) {
  stats::filter(x, rep(1/n,n), method = "convolution", sides = sides)
}
