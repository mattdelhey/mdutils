# '@export

read_query <- function(file) {
    # http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query
    paste(readLines(file), collapse="\n")
}

clear_all <- function() {
    rm(list = ls())
}

unload <- function(package) {
    # http://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r
    name <- paste0("package:", deparse(substitute(package)))
    detach(name = name, unload = TRUE, character.only = TRUE)
}

