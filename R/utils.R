# '@export

any_na <- function(df, dimn = NULL) {
    # For each col in a df, see if there are any NA's in that col.
    dimn <- ifelse(is.null(dimn), 2, dimn)
    apply(df, dimn, function(x) any(is.na(x)))
}

