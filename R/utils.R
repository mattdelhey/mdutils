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
