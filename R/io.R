#' @title read_query
#' @description See: http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query
#' @export
read_query <- function(file) {
    paste(readLines(file), collapse="\n")
}

#' @title clear_all
#' @description TBD
#' @export
clear_all <- function() {
    rm(list = ls())
}

#' @title unload
#' @description See: http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
#' @export
unload <- function(package) {
    name <- paste0("package:", deparse(substitute(package)))
    detach(name = name, unload = TRUE, character.only = TRUE)
}

