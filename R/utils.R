# '@export


clear_all <- function() {
    rm(list = ls())
}

qhist <- function(x) {
    qplot(x, binwidth = (range(x)[2]-range(x)[1]) / nclass.Sturges(x))
}

read_query <- function(file) {
    # http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query
    paste(readLines(file), collapse="\n")
}

any_na <- function(df, dimn = NULL) {
    # For each col in a df, see if there are any NA's in that col.
    dimn <- ifelse(is.null(dimn), 2, dimn)
    apply(df, dimn, function(x) any(is.na(x)))
}

gg_color_hue <- function(n) {
    # http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
}

unload <- function(package) {
    # http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
    name <- paste0("package:", deparse(substitute(package)))
    detach(name = name, unload = TRUE, character.only = TRUE)
}
