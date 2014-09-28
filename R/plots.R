#' @title hist.gg
#' @description \code{hist} plot using ggplot2. Uses the \code{hist} default binwidth calculation.
#' @export
hist.gg <- function(x) {
    label <- ifelse(!is.null(names(x)), names(x), "x")
    x <- unlist(x)
    if (!is.numeric(w)) stop("data must be numeric")
    qplot(x, binwidth = (range(x)[2] - range(x)[1]) / nclass.Sturges(x), geom = "histogram") + xlab(label)
}

#' @title acf.gg
#' @description \code{acf} plot using ggplot2.
#' @export
acf.gg <- function(x) {
    # TODO
}

#' @title ggcolor
#' @description Recreate ggplot2 colors for arbitrary number of factors.
#' See: http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#' @export
ggcolor <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
}
