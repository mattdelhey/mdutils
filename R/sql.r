#' read raw sql query from file. Header is useful for setting hiveconf variables.
#' See: http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query
#' @param file query file
#' @param header optional string to append to beginning of query, automatically adds ';'
#' @export
read.sql.raw <- function(file, header = NULL) {
  qry <- paste(readLines(file), collapse = "\n")
  if (!is.null(header)) qry <- paste(header, qry, sep = ";")  
  return(qry)
}

#' read sql query from file WITH variables and cleaning
#' @inheritParams read.sql.raw
#' @inheritParams var.sub
#' @export
read.sql <- function(file, header = NULL, ...) {  
  qry <- read.sql.raw(file, header)
  qry <- var.sub(qry, ...)
  qry <- clean.sql(qry)
}

#' substitute variables in string
#' @param string input string with variable
#' @param ... variables to replaced of the form "blah" = 5
#' @export
var.sub <- function(string, ...) {
  dots <- list(...)
  # Exit if no replacements
  if(length(dots) == 0) 
    return(string)  
  # Otherwise, loop through replacements
  for(i in 1:length(dots)) 
    string <- gsub(names(dots)[i], dots[[i]], string, fixed = TRUE)  
  return(string)
}

#' clean query text
#' @param qry query text loaded from read.sql
#' @export
clean.sql <- function(qry) {
  tmp <- gsub("\\s{2,}|\n", " ", qry)
  tmp <- gsub("\\( ", "(", tmp)
  tmp <- gsub(" ,", ",", tmp)
  return(tmp)
}

#' collapse vector for use with sql. Default output is string vector.
#' @param x vector to be collapsed
#' @param type output of collapsed vector
#' @param custom optional. collapse using a custom string
#' @param ... further arguments passed to paste
#' @export
collap <- function(x, type = c("character", "numeric", "custom"), custom = NULL, ...) {
  type <- match.arg(type)
  switch(type
       , character = sprintf("'%s'", paste(x, collapse = "', '", ...))
       , numeric   = sprintf("%s",   paste(x, collapse = ",",    ...))
       , custom    = sprintf("%s",   paste(x, collapse = custom, ...))
         )
}

#' generate sql insert statement from dataframe
#' @param data dataframe to import
#' @param table.name name of table in db to insert into
#' @export
insert.sql <- function(data, table.name) {
  sql.data <- paste(apply(data, 1, function(x) 
    paste("('", paste(x, collapse = "', '", sep=''), "')", sep='')), collapse = ", ", sep = '')
  sql.data <- gsub("'NULL'", "NULL", sql.data)
  qry <- sprintf("insert into %s values %s;", table.name, sql.data)
  return(qry)
}

