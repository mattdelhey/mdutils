#' @title qhist
#' @description Implementation of \code{hist} in ggplot2. Uses hist()'s default binwidth calculation.
#' @export
qhist <- function(x) {
    qplot(x, binwidth = (range(x)[2]-range(x)[1]) / nclass.Sturges(x))
}

#' @title ggcolor
#' @description Recreate ggplot2 colors for arbitrary number of factors (stackoverflow). http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' @export
ggcolor <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
}
