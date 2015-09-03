#' Convert proportion to percent string.
#' @param x Proportion to convert.
#' @param digits Number of digits to print.
#' @param format Output format.
#' @family utils
#' @export
format.percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#' Sample n rows from a dataframe. Works just like head/tail.
#' @param x Dataframe to sample.
#' @param n Number of rows to sample.
#' @return Dataframe with n randomly selected rows.
#' @family utils
#' @export
samp <- function(x, n = 6L) {
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
    n_samp <- sample(nrow(x), n)
    x[n_samp, , drop=FALSE]
}

#' For each col in a df, see if there are any NA's in that col.
#' @param df Dataframe to check.
#' @param dimn Dimension to apply on. See ?apply.
#' @return Logical vector of length = number of columns.
#' @family utils
#' @export
any.na <- function(df, dimn = NULL) {
    dimn <- ifelse(is.null(dimn), 2, dimn)
    apply(df, dimn, function(x) any(is.na(x)))
}

#' Subset a vector to its values which are NA.
#' @param x Vector with missing values.
#' @return Subset of vector without missing values.
#' @family utils
#' @export
which.na <- function(x) x[which(is.na(x))]

#' Wrapper for sending email using mutt.
#' @family utils
#' @export
send.email <- function(to=NULL, subject=NULL, body=NULL, attachment=NULL) {
  if (!is.character(to) || is.null(to))
    stop("to argument must be defined and a character!")
  if (!is.character(subject) && !is.null(subject))
    stop("subject argument must be character")
  if (!is.character(body) && !is.null(body))
    stop("body argument must be character")
  if (!is.character(attachment) && !is.null(attachment))
    stop("attachment")
  if (system('mutt -v', ignore.stdout=TRUE) != 0)
    stop("Unable to invoke mutt.")    
  system(sprintf("echo '%s' | mutt -s '%s' -a '%s' -- %s", body, subject, attachment, to))
}

#' Get last element of object.
#' @family utils
#' @export
last <- function(x) head(x, n = -1)

#' Split data into arbitrary partitions
#' @param n number of indicies
#' @param p vector that sums to one
#' @return List of split indicies
#' @family utils
#' @export
split.data <- function(n, p) {
    if (sum(p) != 1) stop("Split proportions must sum to one.")
    if (length(n) != 1) stop("n is the number of indicies")

    k <- length(p)
    s <- sapply(2:k, function(j) floor(p[j] * n))
    s <- c(n - sum(s), s)
    stopifnot(sum(s) == n)
    
    ind <- vector("list", k)
    ind[[1]] <- sample(n, size = s[1], replace = FALSE)
    for (j in 2:k) {
        available <- setdiff(1:n, unique(unlist(ind[1:j])))
        ind[[j]] <- sample(available, size = s[j], replace = FALSE)
    }
    stopifnot(all(sort(unlist(ind)) == 1:n))

    names(ind) <- names(p)
    ind
}

#' Sample variance (unbiased)
#' @param x data vector
#' @param x2 data vector squared
#' @export
sample.var <- function(x, x2) {
  n <- length(x)
  1/(n*(n-1)) * (n*sum(x2) - sum(x)^2)
}

#' bootstrap a statistic for a vector
#' @param x data vector
#' @param t statistic
#' @param n number of simulations
#' @export
bstrap <- function(x, t = mean, n = 10000) {
  replicate(n, {
    t(sample(x, length(x), replace = TRUE))
  })
}

#' normal model (t test) lower/upper bounds for univariate rv with unknown var
#' @param x random variable
#' @param alpha significance level
#' @return vector of lower/upper bounds of realizations of rv
#' @export
model.normal.bounds <- function(x, alpha=0.05) {
  n <- length(x); xbar <- mean(x); s <- sd(x)  
  t.stat <- qt(1-alpha/2, df=n-1)
  bound <- t.stat*(s/sqrt(n))
  c(xbar - bound, xbar + bound)
}

#' binomial model (proportion) lower/upper bounds for univariate rv
#' @param p observed or estimated proportion
#' @param n number of observations
#' @param k number of successes
#' @param alpha signifiance level
#' @param type method of approximation
#' @return bounds
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

#' Chunk date range into given size. The last chunk may contain fewer elements.
#' @param dt.range vector of dates
#' @param dt.start first day for vector construction
#' @param dt.end last day for vector construction
#' @param chunk.size size of each chunk
#' @family utils
#' @export
chunk.ds <- function(dt.range=NULL, dt.start=NULL, dt.end=NULL, chunk.size, reverse = FALSE) {
  if (is.null(dt.range) && (is.null(dt.start) || is.null(dt.end)))
    stop("must supply either dt.range or dt.start and dt.end")
  if (is.null(dt.range))
    dt.range <- seq.Date(from=as.Date(dt.start), to=as.Date(dt.end), by=1)
  if (reverse)
    dt.range <- rev(dt.range)
  split(dt.range, ceiling(seq_along(dt.range)/chunk.size))
}

#' Sphere data. This is a stronger enforcement than scaling as it forces Cov = I.
#' @param x data matrix to be sphered
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
#' @export
rescale <- function(x) {
  apply(x, 2, function(z) (z - min(z)) / diff(range(z)))
}

#' Center a numeric vector by subtracting off its mean.
#' @param x numeric vector
#' @export
center <- function(x) {
  scale(x, center = TRUE, scale = FALSE)
}

#' Cast specific columns as numeric
#' @param x data.frame
#' @param cols column indicies to be converted to numeric
as.numeric.cols <- function(x, cols) {
  for (j in numeric.cols) {
    x[, j] <- as.numeric(x[, j])
  }
  x
}

#' Sort by absolute value using base::sort
#' @param x vector be sorted
#' @inheritParams sort
sort.abs <- function(x, ...) {
  old.names <- names(x)
  names(x) <- 1:length(x)
  ind <- names(sort(abs(x), ...))
  res <- x[ind]
  names(res) <- old.names
  res
}

#' Summarize a coefficient matrix
#' @param beta matrix of coefficient where rows represent variables and columns represent hyperparameter
summarize.coefficient.matrix <- function(beta) {
  beta.bar <- rowMeans(beta)
  data.frame(
    variable = dimnames(beta)[[1]]
  , beta = sort.abs(beta.bar, decreasing = TRUE)
  , sign = sign(beta.bar))
}

#' Generic dispatch for importance
#' @param x model fit object with correct class
importance <- function(x, ...) {
  UseMethod("importance", x)
}

#' Variable importance for lm fit
#' Importance determined by absolute value of t-statistic
#' @param x lm fit
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
importance.glmnet <- function(x, ...) {
  beta <- x$beta
  summarize.coefficient.matrix(beta)
}

#' Variable importance for cv.glmnet fit
#' Importance determined by absolute value of mean coefficient
#' @param x cv.glmnet fit
importance.cv.glmnet <- function(x, ...) {  
  beta <- x$glmnet.fit$beta
  summarize.coefficient.matrix(beta)
}

#' Add bias column to data matrix
#' @param x data matrix
add.bias <- function(x) {
  stopifnot(is.matrix(x) || is.data.frame(x))
  n <- nrow(x)
  res <- cbind(cbind(rep(1, n)), x)
  if (is.data.frame(x)) res <- as.data.frame(res)
  res
}

