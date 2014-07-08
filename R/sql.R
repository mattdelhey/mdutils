#' @title read_query
#' @description Read sql query from file. Header is useful for setting hiveconf variables.
#' See: http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query
#' @export
read_query <- function(file, header = NULL) {
    qry <- paste(readLines(file), collapse = "\n")
    if (!is.null(header)) qry <- paste(header, qry, sep = ";")
    return(qry)
}

#' @title collapse_vec
#' @description Collapse R vector for use with sql. Default output is string vector.
#' @export
collapse_vec <- function(x, type = c("character", "numeric")) {
    arg_type <- match.arg(type)    
    if (arg_type == "character")
        sprintf("'%s'", paste(x, collapse = "', '"))
    if (arg_type == "numeric")
        sprintf("%s", paste(x, collapse = ","))
}
