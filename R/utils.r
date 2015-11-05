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

#' Cast specific columns as numeric
#' @param x data.frame
#' @param cols column indicies to be converted to numeric
#' @family utils
#' @export
as.numeric.cols <- function(x, cols) {
  for (j in numeric.cols) {
    x[, j] <- as.numeric(x[, j])
  }
  x
}

#' Sort by absolute value using base::sort
#' @param x vector be sorted
#' @inheritParams sort
#' @family utils
#' @export
sort.abs <- function(x, ...) {
  old.names <- names(x)
  names(x) <- 1:length(x)
  ind <- names(sort(abs(x), ...))
  res <- x[ind]
  names(res) <- old.names
  res
}

#' Coalesce two or more vectors
#' Replaces missing values of first vector in order of remaining vectors
#' @param ... vectors to coalesce
#' @family utils
#' @export
coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
  list(...))
}

#' Relative cummulative summation
#' @param x vector
#' @family utils
#' @export
relcumsum <- function(x) {
  y <- cumsum(x)
  last <- tail(y, 1)
  coalesce(y / last, 0)
}

#' Relative cummulative summary
#' @param x cumsum vector
#' @family utils
#' @export
relcum <- function(x) x / tail(x,1)

roundup <- function(x) plyr::round_any()

#' @title clear_all
#' @description Remove all objects in workspace.
#' @family utils
#' @export
clear_all <- function() {
    rm(list = ls())
}

#' @title unload
#' @description Unload a package using library syntax.
#' See: http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
#' @family utils
#' @export
unload <- function(package) {
    name <- paste0("package:", deparse(substitute(package)))
    detach(name = name, unload = TRUE, character.only = TRUE)
}

#' compound revenue for a given period
#' @family utils
#' @export
compound_revenue <- function(revenue, growth_rate) {
  n <- length(revenue)
  growth_factor <- c(growth_rate, rep(NA, n-1)) ## init growth with growth_rate
  for (i in 2:n)
    growth_factor[i] <- (((100+growth_factor[i-1])/100 * (100+growth_factor[1])/100)-1)*100  
  revenue_goal <- revenue * (1+growth_factor/100)
  data.frame(period = 1:n, growth_rate = growth_rate, growth_factor = growth_factor, revenue_goal = revenue_goal)
}

#' determine growth within month
#' @family utils
#' @export
extrapolate_days <- function(revenue_goal, ndays, percent_change = 3.5) {
  mean <- revenue_goal / ndays
  delta <- mean * percent_change/100
  min <- mean - delta
  max <- mean + delta
  factor <- (max - min) / ndays
  revenue_goal_per_day <- c(min, rep(NA, ndays-1))
  for (i in 2:ndays)
    revenue_goal_per_day[i] <- revenue_goal_per_day[i-1] + factor
  if (!all.equal(sum(revenue_goal_per_day), revenue_goal, tolerance = revenue_goal * 0.0001))
    stop("daily revenue goal does not add up to monthly revenue goal")
  revenue_goal_per_day
}

#' expand monthly revenue to daily revenue
#' @family utils
#' @export
extrapolate_month <- function(revenue_goal, month, year = "2016", day = "1") {
  first_date <- as.Date(paste(year, month, day, sep = "-"))
  ndays <- days_in_month(first_date)
  last_date <- as.Date(paste(year, month, ndays, sep = "-"))
  revenue_goal_per_day <- revenue_goal / ndays
  ##extrapolate_days(revenue_goal, ndays, 3.5)
  names(revenue_goal_per_day) <- NULL
  data.frame(date = seq(from = first_date, to = last_date, by = 1), revenue_goal = revenue_goal_per_day)
}
