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
send.email <- function(to, subject, body, attachment) {
    if (!(is.character(to) && is.character(subject) && is.character(body) && is.character(attachment)))
        stop("All arguments must be strings.")
    if (system('mutt -v') != 0)
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
sample.var <- function(x, x2) {
  n <- length(x)
  1/(n*(n-1)) * (n*sum(x2) - sum(x)^2)
}

#' bootstrap a statistic for a vector
#' @param x data vector
#' @param t statistic
#' @param n number of simulations
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
chunk.ds <- function(dt.range=NULL, dt.start=NULL, dt.end=NULL, chunk.size) {
  if (is.null(dt.range) || (is.null(dt.start) || is.null(dt.end)))
    stop("must supply either dt.range or dt.start and dt.end")
  if (is.null(dt.range))
    dt.range <- seq.Date(from=as.Date(dt.start), to=as.Date(dt.end), by=1)
  split(dt.range, ceiling(seq_along(dt.range)/chunk.size))
}
