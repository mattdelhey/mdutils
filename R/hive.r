## Init hive envrionment and default values
hive_env <- new.env()
hive_env$cluster <- "adhoc"
hive_env$warehouse_path <- "/home/site/warehouse"
hive_env$header <- FALSE

#' run hive command
#' @param cmd hive command
#' @param fileout temporary output file
#' @param isfile is the input a file?
#' @return stdout
#' @export
hive <- function(cmd, fileout = tempfile("Rtmp_", fileext = ".dat"), isfile = FALSE) {
  ## Is entry a command or a filename?
  if (isfile) {
    hive_cmd <- sprintf("hive -f %s", cmd)
  } else {
    fn_query <- tempfile("Rtmp_", fileext = ".tmp", tmpdir = "/tmp")
    fail_message <- sprintf("Failed to write to temporary file: %s", fn_query)
    zero_check(write(cmd, fn_query), quit = FALSE,  message = fail_message)
    hive_cmd <- sprintf("hive -f %s", fn_query)
  }
  ## Is output to stdout of file?
  if(!is.na(fileout)) {
    hive_cmd <- paste(hive_cmd, sprintf('> %s', fileout))
    hive_system(hive_cmd)
  }
  else {
    hive_system(hive_cmd, intern = TRUE)
  }
}

#' run hive command without return
#' @inheritParams hive
#' @return void!
#' @export
hive_void <- function(cmd, ...) {
  if(nargs() == 1)
    hive(cmd, fileout = NA)
  else
    hive(sprintf(cmd, ...), fileout = NA)
}

#' run hql file without return
#' @inheritParams hive
#' @export
hive_void_file <- function(file) hive(file, fileout = NA, isfile = TRUE)

#' parse and shift first row as header
#' @param x dataframe
#' @export
parse_header <- function(x) {
  names(x) <- x[1, ]
  x[2:nrow(x), ]
}

#' logic for special cases
#' @export
#' @family hive
set_tempfile <- function(tmpdir = "/tmp", fileext = ".tmp") {
  hostname <- get_hostname()
  if (hostname == "kant.dhcp.tripadvisor.com" && tmpdir == "/tmp")
    tmpdir_use <- "/data/tmp"
  else
    tmpdir_use <- tmpdir
  tempfile(tmpdir = tmpdir_use, fileext = fileext)
}

#' retrieve hive query results as data.frame
#' WARNING: This function will throw an error if tabs appear in field data
#' @param qry hive query to be executed (character vector)
#' @param init
#' @export
#' @family hive
get_hive <- function(qry = NULL, init.qry = "", isfile = FALSE, queue = "sem",
                     fn_tmp = set_tempfile(tmpdir = "/tmp", fileext = ".dat")) {
  ## CHECK: Was a command specified?
  if (is.null(qry)) log_error("Query not valid: %s", qry)
  ## Set Hive option for header
  init.qry <- add_sql(sprintf("SET hive.cli.print.header=true; SET mapred.job.queue.name=%s;", queue), init.qry)  
  ## Run Hive query
  log_info("Initiating Hive query.  Storing temp results to %s", fn_tmp)
  timer_start()
  if (isfile) {
    hive(qry, fileout = fn_tmp, isfile = TRUE)
  } else {
    hive(add_sql(init.qry, qry), fileout = fn_tmp)
  }
  log_info("Hive query completed in: %s.  Temp results stored to %s", timer_stop(), fn_tmp)
  ## Check that temp file exists and contains data
  if(!file.exists(fn_tmp) | length(readLines(fn_tmp, n = 1)) < 1) {
    log_error("No query results returned!")
  }
  ## Convert Hive output to data frame
  log_info("Converting Hive output to data frame")
  out <- read.table(fn_tmp, header = TRUE, sep = "\t", quote = "", comment.char = "", skipNul = TRUE)
  ## Return data frame of Hive results
  log_info("Returning data frame containing %s rows and %s columns", nrow(out), ncol(out))
  return(out)
}

#' read hive table to r data.frame
#' @param tbl hive table name
#' @export
#' @family hive
get_hive_tbl <- function(tbl, add.qry = "", init.qry = "") {    
  ## Collapse SQL options
  init.qry <- add_sql("set hive.cli.print.header=true", init.qry)
  ## Query table description
  log_info("Retrieving metadata for table: %s", tbl)
  tbl.desc <- hive(sprintf("%s; DESCRIBE %s", init.qry, tbl), fileout = NA)
  ## Convert metadata to data frame
  meta <- parse_header(do.call(rbind.data.frame, strsplit(tbl.desc, "\t")))
  ## Generate select statement with delimiter replacement on string fields
  select.str <- meta[, "col_name"]
  select.str.ori <- select.str
  str.idx <- meta[, "data_type"] == "string"
  select.str[str.idx] <- sprintf("regexp_replace(%s, '\t', ' ') as %s", 
                                 select.str[str.idx], select.str[str.idx])
  select.str <- paste(select.str, collapse = ", ")
  ## Run completed query
  log_info("Retrieving data from: %s", tbl)
  ## Return output
  get_hive(sprintf("%s; SELECT %s from (SELECT %s FROM %s %s) x;", init.qry, select.str,
                   paste(select.str.ori, collapse = ", "), tbl, add.qry))
}

#' run the hive system command with specific envrionment
#' @param hive_cmd command to be sent to hive
#' @export
#' @family hive
hive_system <- function(hive_cmd, quit = FALSE, ...) {
  path_env <- file.path(hive_env$warehouse_path, "clusters", hive_env$cluster, "config", "env.bash")
  path_cluster <- file.path(hive_env$warehouse_path, "clusters", hive_env$cluster)
  setup <- sprintf(". %s %s && ", path_env, path_cluster)
  cmd <- paste(setup, hive_cmd)
  message(cmd)
  ##zero_check(system2("bash", args = c("-c", shQuote(cmd)), ...), quit = quit)
  ##zero_check(system(sprintf("sudo -E -u sem bash -c %s", cmd), ...), quit = quit)
  ##system(sprintf("sudo -E -u sem %s", cmd), ...)
  user <- Sys.info()["user"]
  if (user != "sem") {
    system2("sudo", c("-u sem -s bash -c", shQuote(cmd)))
  } else {
    system2("bash", c("-c", shQuote(cmd)))
  }
}

#' set warehouse path
#' @param path
#' @export
#' @family hive
set_warehouse_path <- function(path) {
  hive_env$warehouse_path <- path
}

#' set hive cluster
#' @param cluster name of cluster to use with hive
#' @return given cluster name
#' @export
#' @family hive
set_hive_cluster <- function(cluster = c("prod", "adhoc", "cdhtest", "core", "leo", "test")) {
  cluster <- match.arg(cluster)
  hive_env$cluster <- cluster
}

#' get column names of a hive table for tar.hive
#' @export
get_hive_header <- function(tbl) {
  data <- hive.query(sprintf("describe %s", tbl), silent = TRUE)
  data_clean <- apply(data, 2, stringr::str_trim)
  data_types <- c("string", "int", "double", "void", "boolean", "bigint", "float", "varchar", "timestamp", "date", "binary", "decimal")
  ## remove partition rows
  has_partition <- any(grepl("Partition", data_clean[, 1]))
  if (has_partition) data_clean <- data_clean[-(which(grepl("Partition", data_clean[, 1])):nrow(data)), ]
  rows <- which(data_clean[, 2] %in% data_types)
  column_names <- as.character(data_clean[rows, 1])
}

#' check if R process has sudo power
#' @export
check_sudo <- function() {  
}

#' check if adequate disk space exists
#' @export
check_disk <- function(path) {
}

#' get disk space
#' https://stat.ethz.ch/pipermail/r-help/2007-October/142319.html
#' @export
#' @family hive
get_disk <- function(path = Sys.getenv("HOME")) {
   if(length(system("which df", intern = TRUE, ignore = TRUE))) {
     cmd <- sprintf("df %s", path)
     exec <- system(cmd, intern = TRUE, ignore = TRUE)
     exec <- strsplit(exec[length(exec)], "[ ]+")[[1]]
     exec <- as.numeric(exec[3:4])
     structure(exec, names = c("used", "available"))
   } else {
     stop("'df' command not found")
   }
}

