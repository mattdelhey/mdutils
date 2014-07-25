#' Convert proportion to percent string.
#' @param x Proportion to convert.
#' @param digits Number of digits to print.
#' @param format Output format.
#' @family utils
#' @export
percent <- function(x, digits = 2, format = "f", ...) {
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


#' #' Wrapper for sending email using mutt.
#' @family utils
#' @export
send_email <- function(to, subject, body, attachment) {
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
