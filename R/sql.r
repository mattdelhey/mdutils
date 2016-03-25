#' read raw sql query from file. Header is useful for setting hiveconf variables.
#' See: http://stackoverflow.com/questions/3580532/r-read-contents-of-text-file-as-a-query
#' @param file query file
#' @param header optional string to append to beginning of query, automatically adds ';'
#' @family sql
#' @export
read_sql_raw <- function(file, header = NULL) {
  qry <- paste(readLines(file), collapse = "\n")
  if (!is.null(header)) qry <- add_sql(header, qry)
  qry
}

#' Add additional SQL command or SET
#' @param current_sql sql to be added to
#' @param new_sql sql to be added from
#' @family sql
#' @export
add_sql <- function(current_sql, new_sql) paste(current_sql, new_sql, sep = ";")

#' read sql query from file WITH variables and cleaning
#' @inheritParams read_sql_raw
#' @inheritParams var_sub
#' @family sql
#' @export
read_sql <- function(file, header = NULL, ...) {  
  qry <- read_sql_raw(file, header)
  qry <- var_sub(qry, ...)
  qry <- clean_sql(qry)
}

#' substitute variables in string
#' @param string input string with variable
#' @param ... variables to replaced of the form "blah" = 5
#' @family sql
#' @export
var_sub <- function(string, ...) {
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
#' @param qry query text loaded from read_sql
#' @family sql
#' @export
clean_sql <- function(qry) {
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
#' @family sql
#' @export
collap <- function(x, type = c("character", "numeric", "custom"), custom = NULL, ...) {
  type <- match.arg(type)
  switch(type
       , character = sprintf("'%s'", paste(x, collapse = "', '", ...))
       , numeric   = sprintf("%s",   paste(x, collapse = ",",    ...))
       , custom    = sprintf("%s",   paste(x, collapse = custom, ...))
         )
}

#' alias 'to easily collapse (character) vector to hive (string) list'
#' @inheritParams collap
#' @family sql
#' @export
collapse_char_vec <- function(x) {
  collap(x, type = "character")
}

#' generate sql insert statement from dataframe
#' @param data dataframe to import
#' @param table.name name of table in db to insert into
#' @family sql
#' @export
insert.sql <- function(data, table.name) {
  sql.data <- paste(apply(data, 1, function(x) 
    paste("('", paste(x, collapse = "', '", sep=''), "')", sep='')), collapse = ", ", sep = '')
  sql.data <- gsub("'NULL'", "NULL", sql.data)
  qry <- sprintf("insert into %s values %s;", table.name, sql.data)
  return(qry)
}

#' get all rows from a table
#' @param con dbi connection
#' @param x table name
#' @family sql
#' @export
get_tbl <- function(con=NULL, x, hive=FALSE) {
  stopifnot("dplyr" %in% rownames(installed.packages()))
  if (!hive) {
    if (is.null(con)) stop("con cannot be null")
    return(as.tbl(dbGetQuery(con, sprintf("select x.* from %s x", x))))
  }
  if (hive)
    return(as.tbl(get_hive(sprintf("select * from %s", x))))
}

#' get all rows from a table
#' @inheritParams get_tbl
#' @family sql
#' @export
get_htbl <- function(x) {
  stopifnot("dplyr" %in% rownames(installed.packages()))
  as.tbl(get_hive(sprintf("select * from %s", x)))
}

#' get query
#' @param con dbi connection
#' @param qry query text
#' @family sql
#' @export
get_qry <- function(con, qry, ...) {
  stopifnot("dplyr" %in% rownames(installed.packages()))
  as.tbl(dbGetQuery(con, qry))
}


#' save r data.frame to sql table
#' @param con dbi connection
#' @param tn output table name
#' @param tbl r table
#' @inheritParams RPostgreSQL::dbWriteTable
save_tbl <- function(con, tn, tbl, overwrite=TRUE, append=FALSE, row.names=FALSE, ...) {
  info <- dbGetInfo(con)
  log_message(vsub("saving to [host: %s0] [db: %s1] [tbl: %s2]", "%s0" = info$host, "%s1"=info$dbname, "%s2"=tn))
  dbWriteTable(con, tn, as.data.frame(tbl), row.names=row.names, overwrite=overwrite, append=append, ...)
}


#' transfer from swt to tamp
#' @param con output database connection
#' @param tn_in input table name
#' @param tn_out output table name
#' @param swtparam location of sem warehouse param file
transfer <- function(con, tn_in, tn_out, swtparams="../swtparams.r") {
  library(RJDBC)
  source(swtparams)
  tbl <- get_tbl(swt.con, tn_in)
  invisible(save_tbl(con, tn_out, tbl))
}

