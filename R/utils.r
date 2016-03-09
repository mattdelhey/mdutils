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
any.na <- function(x) UseMethod("any.na", x)
any.na.numeric <- function(x) any(is.na(x))
any.na.data.frame <- function(df, dimn = NULL) {
    dimn <- ifelse(is.null(dimn), 2, dimn)
    apply(df, dimn, function(x) any(is.na(x)))
}

#' Subset a vector to its values which are NA.
#' @param x Vector with missing values.
#' @return Subset of vector without missing values.
#' @family utils
#' @export
which.na <- function(x) x[which(is.na(x))]

#' Subset a vector to its values which are INF
#' @param x Vector with missing values
#' @return Subset of vector without missing values
#' @family utils
#' @export
which.inf <- function(x) x[which(is.infinite(x))]

#' Replace NA values in vector
#' @param x data.frame
#' @family utils
#' @export
repl.na <- function(x, ...) UseMethod("repl.na", x)
repl.na.data.frame <- function(x, repl) {
  apply(x, 2, repl.na, repl)
}
repl.na.matrix <- function(x, repl) {
  x[which.na(x), ] <- repl
  x
}
repl.na.numeric <- function(x, repl) {
  x[which.na(x)] <- repl
  x
}
repl.na.character <- function(x, repl) {
  x[which.na(x)] <- repl
  x
}


#' Replace Inf values in vector
#' @param x data.frame
#' @family utils
#' @export
repl.inf.data.frame <- function(x, repl) {
  apply(x, 2, repl.inf, repl)
}
repl.inf.data.frame <- function(x, repl) {
  x[which.inf(x), ] <- repl
  x
}

#' Calculate fortnight-in-year. Similar to lubridate::week
#' @param x date-time
#' @family utils
#' @export
fortnight <- function(x) nperiod(x, ndays = 14)

#' Calculate period-in-year where period can be defined by an arbitrary number of days (e.g. 14)
#' @param x date-time object
#' @param ndays number of days in period
#' @family utils
#' @export
nperiod <- function(x, ndays = 7) (lubridate::yday(x) - 1) %/% ndays + 1
##x <- x + days((value - week(x)) * 7)

#' Wrapper for sending email using mutt.
#' @family utils
#' @export
send_email <- function(to=NULL, cc=NULL, subject=NULL, body=NULL, attachment=NULL) {
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
  if (!is.null(attachment) && !is.null(body) && !is.null(subject))
    system(sprintf("echo '%s' | mutt -s '%s' -a '%s' -- %s", body, subject, attachment, to))
  if (!is.null(body) && is.null(cc) && is.null(attachment))
    system(sprintf("echo '%s' | mutt -s '%s' -- %s", body, subject, to))
  if (!is.null(cc) && is.null(attachment)) {
    cc_vec <- sprintf("-c %s", paste(cc, collapse = " -c "))
    system(sprintf("echo '%s' | mutt -s '%s' %s -- %s", body, subject, cc_vec, to))
  }
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
#' @inheritParams base::sort
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

#' create a null data frame that contains a value for each combination of factors
#' @param x data frame
#' @param factors vector of factor column names; filled by unique values in data frame
#' @param measures vector of measures column names; filled by "fill"
#' @param fill default fill value for measure columns
#' @family utils
#' @export
null_frame <- function(x, factors, measures, fill = NA) {
  ## unique combinations of all factors
  levels <- sapply(factors, function(j) unique(x[, j]))
  comb <- expand.grid(levels)
  names(comb) <- factors
  ## measure columns
  measure_frame <- data.frame(matrix(fill, ncol = length(measures), nrow = nrow(comb)))
  names(measure_frame) <- measures
  ## combine
  cbind(comb, measure_frame)
}

#' join with null data frame, coalesce measure columns
#' @family utils
#' @export
null_join <- function(x, null, factors, measures) {
  y <- suppressWarnings(left_join(null, x, by = factors))
  ## For each column, get the two same-name vectors and coalesce
  measures_coalesced <- sapply(measures, function(m) {
    measure <- y[, grepl(m, names(y))]
    ## only works for built-in numeric types, so force it
    coalesce(as.vector(as.matrix(measure[, 2])), as.vector(as.matrix((measure[, 1]))))
  })
  y_measures_removed <- y[, factors] ## probably not the best way to do this, assumes y = factors + measures
  cbind(y_measures_removed, measures_coalesced)
}

#' safe divide
#' @param x numerator
#' @param y denominator
#' @param fail replacement for when denominator is zero
#' @family utils
#' @export
safe_divide <- function(x, y, fail = 0) {
  ifelse(y == 0, fail, x/y)
}

#' sum last l values
#' @param l number of values back to sum
sumlast <- function(x, l, order_by = NULL, ...) {
  lag <- length(x) - (l-1)
  use <- max(0, lag)
  last <- dplyr::lag(x, use, default = NA, order_by = order_by, ...)
  sum(last, na.rm = TRUE)
}

#' extend dplyr to multiple lags
#' @param lags vector of lags to be computed
#' @param reduce function to reduce results
#' @inheritParams dplyr::lag
#' @inheritParams base::do.call
#' @family utils
#' @export
lags <- function(x, lags = 1L:2L, default = NA, order_by = NULL, ...) {
  laglist <- lapply(lags, function(l) dplyr::lag(x, l, default = default, order_by, ...))
  Reduce(function(x) sum(x, na.rm=TRUE), laglist)
  ##Reduce(function(reduce, dots) reduce(x, dots))
  ##Reduce(function(reduce, reduce_args) do.call("reduce", reduce_args), container)
}

#' find system hostname
#' taken from R.utils package
#' @family utils
#' @export
get_hostname <- function() {
  host <- Sys.getenv(c("HOST", "HOSTNAME", "COMPUTERNAME"));
  host <- host[host != ""];
  if (length(host) == 0) {
    # Sys.info() is not implemented on all machines, if not it returns NULL,
    # which the below code will handle properly.
    host <- Sys.info()["nodename"];
    host <- host[host != ""];
    if (length(host) == 0) {
      host <- readLines(pipe("/usr/bin/env uname -n"));
    }
  }
  host[1]
}

#' find system username
#' taken from R.utils pacakge
#' @family utils
#' @export
get_username <- function() {
  user <- Sys.getenv(c("USER", "USERNAME"));
  user <- user[user != ""];
  if (length(user) == 0) {
    # Sys.info() is not implemented on all machines, if not it returns NULL,
    # which the below code will handle properly.
    user <- Sys.info()["user"];
    user <- user[user != "" & user != "unknown"];
    if (length(user) == 0) {
      user <- readLines(pipe("/usr/bin/env whoami"));
    }
  }
  user[1]
}

#' Variable substitution
#' Substitute named variables
#' @examples
#' vsub("test_%s1_%s2", "%s1" = 6, "%s2" = 10)
#' @family utils
#' @export
vsub <- function(txt, clean=FALSE, ...) {
  dots <- list(...)
  # Exit if no replacements
  if(length(dots) == 0) 
    return(txt)
  # Otherwise, loop through replacements
  for(i in 1:length(dots)) 
    txt <- gsub(names(dots)[i], dots[[i]], txt, fixed = TRUE)
  if (clean)
    txt <- clean_sql(txt)
  return(txt)
}

#' proportional table
#' @export
ptable <- function(x) table(x) / sum(table(x))

#' display random elements or rows
#' @family utils
#' @export
rand <- function(x, ...) UseMethod("rand", x)
rand.numeric <- function(x, n=6, ...) x[sample(1:length(x), n)]
rand.data.frame <- function(x, n=6, ...) x[sample(1:nrow(x), n), ]

#' check if packaged is installed
package_installed <- function(x)
  x %in% rownames(installed.packages())

#' check if package is loaded
package_loaded <- function(x)
  x %in% names(sessionInfo()$loadedOnly)

#' dplyr summary simplifier
#' summarizes columns using a single aggregate function
summarizeff <- function(x, f, metrics) {
  if (!is.character(f) || !is.character(metrics))
    stop("f and metrics must be specified as character vector")
  if (!package_installed("dplyr"))
    stop("dplyr not installed")
  dots <- lapply(metrics, function(m) sprintf("%s(%s)", f, m))
  dots <- setNames(dots, metrics)
  summarize_(x, .dots=dots)
}

as.vec <- function(x) {
  as.vector(as.matrix(x))
}
