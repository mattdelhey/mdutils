#' @title samp
#' Sample n rows from a dataframe. Works just like head/tail.
#' @export
samp <- function(x, n = 6L) {
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) max(nrow(x) + n, 0L) else min(n, nrow(x))
    n_samp <- sample(nrow(x), n)
    x[n_samp, , drop=FALSE]
}

#' @title any_na
#' For each col in a df, see if there are any NA's in that col.
#' @export
any_na <- function(df, dimn = NULL) {
    dimn <- ifelse(is.null(dimn), 2, dimn)
    apply(df, dimn, function(x) any(is.na(x)))
}


#' @title send_email
#' @description Wrapper for sending email using mutt.
#' @export
send_email <- function(to, subject, body, attachment) {
    if (!(is.character(to) && is.character(subject) && is.character(body) && is.character(attachment)))
        stop("All arguments must be strings.")
    if (system('mutt -v') != 0)
        stop("Unable to invoke mutt.")
    
    system(sprintf("echo '%s' | mutt -s '%s' -a '%s' -- %s", body, subject, attachment, to))
}


#' @title last
#' @description Get last element of object.
#' @export
last <- function(x) head(x, n = -1)
