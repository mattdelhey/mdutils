#' qhist
#' ggplot2 implementation of hist(); uses hist()'s default binwidth calculation
#' @export
qhist <- function(x) {
    qplot(x, binwidth = (range(x)[2]-range(x)[1]) / nclass.Sturges(x))
}

#' ggcolor
#' http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' @export
ggcolor <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
}
