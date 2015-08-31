#' @title clear_all
#' @description Remove all objects in workspace.
#' @export
clear_all <- function() {
    rm(list = ls())
}

#' @title unload
#' @description Unload a package using library syntax.
#' See: http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
#' @export
unload <- function(package) {
    name <- paste0("package:", deparse(substitute(package)))
    detach(name = name, unload = TRUE, character.only = TRUE)
}

