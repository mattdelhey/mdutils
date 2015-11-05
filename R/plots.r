#' fixed_breaks
#' @param x vector to break
#' @param by how often to break
fixed_breaks <- function(x, by = 7) seq(min(x), max(x), by = by)

#' theme_tufte
#' \url{https://github.com/mattdelhey/ggthemes/blob/master/R/tufte.R}
#' @param base_size base font size
#' @param base_family base font family
#' @param ticks
#' @export
theme_tufte <- function(base_size = 11, base_family = "serif", ticks=TRUE) {
  thm <- theme_bw(base_family = base_family, base_size = base_size)
  thm <- thm +
    theme(
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      axis.line         = element_blank(),
      panel.grid        = element_blank()
    )
  if (!ticks)
    thm <- thm + theme(axis.ticks = element_blank())
}

#' theme_mdelhey
#' Minimal ggplot2 theme based off of theme_tufte and theme_bw
#' @param base_size base font size
#' @param base_family base font family
#' @param ticks
#' @export
theme_mdelhey <- function(base_size = 12, base_family = "sans", ticks=TRUE) {
  theme_bw(base_family = base_family, base_size = base_size) %+replace%
    theme(
      legend.background = element_blank()
    , legend.key        = element_blank()
    , panel.background  = element_blank()
    , strip.background  = element_blank()
    , plot.background   = element_blank()
    , axis.ticks.length = grid::unit(0.5, "cm")
    , axis.line         = element_line(size = 0.2, color = "#555555")
    , axis.ticks        = element_line(color = "grey90", size = 0.2)
    , panel.border      = element_blank()
    , panel.grid.major  = element_line(color = "grey90", size = 0.2)
      ##, panel.grid.minor  = element_line(color = "grey90", size = 0.2) #grey90
    , panel.grid.minor  = element_blank()
    , plot.title        = element_text(color = "#555555", face = "bold", size = rel(1.33), hjust = 0.01, vjust = 2)
    , plot.margin       = grid::unit(c(2, 0.5, 0, 0.5), "lines") ##unit(c(1, 1, 1, 1), "lines")
    , legend.position   = "bottom"
    , legend.background = element_blank()
    , legend.direction  = "horizontal"
    , legend.box        = "vertical"
    , legend.title      = element_text(face = "bold", color = "#555555")
    , legend.text       = element_text(hjust = 1)
    )
}


#' gghist
#' \code{hist} plot using ggplot2 using the \code{hist} default binwidth calculation
#' @param x Numeric data frame or vector to be plotted
#' @return Histogram
#' @export
gghist <- function(x) {
    label <- ifelse(!is.null(names(x)), names(x), "x")
    x <- unlist(x)
    if (!is.numeric(w)) stop("data must be numeric")
    qplot(x, binwidth = (range(x)[2]-range(x)[1]) / nclass.Sturges(x), geom = "histogram") +
        xlab(label)
}

#' ggacf
#' \code{acf} plot using ggplot2
#' See: \url{http://stackoverflow.com/questions/17788859/acf-plot-with-ggplot2-setting-width-of-geom-bar}
#' @export
ggacf <- function(x, type = c("correlation", "covariance", "partial"), ci = 0.95, ...) {
    type <- match.arg(type)
    x.acf <- acf(x, type = type, plot = FALSE, ...)
    x.df <- with(x.acf, data.frame(lag, acf))
    x.ci <- qnorm((1 + ci)/2) / sqrt(length(x))
    print(x.ci)
    ggplot(data = x.df, aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0)) +
        geom_segment(aes(xend = lag, yend = 0)) +
        geom_hline(yintercept = c(x.ci, -x.ci), color = "blue", linetype = "dashed") +
        scale_y_continuous(breaks = seq(0.0, 1.0, 0.2))
}

#' ggcolor
#' Recreate ggplot2 colors for arbitrary number of factors
#' See: \url{http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette}
#' @export
ggcolor <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
}
