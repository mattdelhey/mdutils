#' zero check
#' @param expr
#' @export
zero_check <- function(expr, quit = TRUE, message = NULL, fail_on_null = FALSE) {
  if (!is.logical(fail_on_null)) stop("fail_on_null must be logical")
  if (!is.logical(quit)) stop("quit must be logical")
  out <- try_log(expr)
  if (is.null(out)) {
    out <- ifelse(fail_on_null, FALSE, TRUE)
  }
  if (length(out) != 1 && !is.null(out)) {
    if (is.null(message)) {
      message <- "Expression %s returned more than one return code"
    }
    log_error(
      message
    , quit = quit
    , deparse(substitute(expr)))
  } else if ((out != 0 && !is.logical(out)) || (is.logical(out) && !out)) {
    if (is.null(message)) {
      message <- "Expression %s had a non-zero return."
    }
    log_error(
      message
    , quit = quit
    , deparse(substitute(expr)))
  }
}

#' memory usage
#' @return current r memory usage
#' @export
#' @family log
mem_usage <- function() {
    show_bytes(sum(gc()[, 1] * c(node_size(), 8)))
}

#' show bytes
#' byte size of r object
#' @param x object
#' @return byte size as numeric
#' @export
#' @family log
show_bytes <- function(x) {
    structure(x, class = "bytes")
}

#' node size
#' @export
#' @family log
node_size <- function() {
    bit <- 8L * .Machine$sizeof.pointer
    if (!(bit == 32L || bit == 64L)) {
        stop("Unknown architecture", call. = FALSE)
    }
    
    if (bit == 32L) 28L else 56L
}

#' Determine change in memory from running code
#' @param code Code to evaluate.
#' @return Change in memory (in megabytes) before and after running code.
#' @examples
#' # Need about 4 mb to store 1 million integers
#' mem_change(x <- 1:1e6)
#' # We get that memory back when we delete it
#' mem_change(rm(x))
#' @export
#' @family log
mem_change <- function(code) {
    start <- mem_usage()
    
    expr <- substitute(code)
    eval(expr, parent.frame())
    rm(code, expr)
    
    show_bytes(mem_usage() - start)
}

#' print bytes
#' @export
print.bytes <- function(x, digits = 3, ...) {
    power <- min(floor(log(abs(x), 1000)), 4)
    if (power < 1) {
        unit <- "B"
    } else {
        unit <- c("kB", "MB", "GB", "TB")[[power]]
        x <- x / (1000 ^ power)
    }
    
    formatted <- format(signif(x, digits = digits), big.mark = ",",
                        scientific = FALSE)
    
    cat(formatted, " ", unit, "\n", sep = "")
}

#' generic logging
#' @param msg
#' @param type
#' @param showMem
#' @export
#' @family log
log_message <- function(msg = "", ..., type = "INFO", showMem = TRUE) {
    if(showMem) {
        cat(sprintf("%s | [%s] | %s | %s\n", Sys.time(), type, capture.output(mem_usage()), sprintf(msg, ...)))
    } else {
        cat(sprintf("%s | [%s] | %s\n", Sys.time(), type, sprintf(msg, ...)))
    }
}

#' info logging
#' alias for log_message
#' @export
#' @family log
log_info <- log_message

#' warn logging
#' @export
#' @family log
log_warn <- function(msg, ...) log_message(msg, ..., type = "WARN")

#' error logging
#' @export
#' @family log
log_error <- function(msg, quit = TRUE, ...) {
  log_message(msg, ..., type = "FATAL ERROR")
  opt <- options(show.error.messages=FALSE)
  on.exit(options(opt))
  if (quit) {
    quit(save = "no", status = 1)
  }
}

#' start function timer
#' @export
timer_start <- function () {
  time_marker <<- Sys.time()
}

#' stop function timer
#' @return running time since timer_start
#' @export
timer_stop <- function() {
  diff <- format(Sys.time() - time_marker)
  return(diff)
}

#' try logging
#' Try something and exit with a log message if it doesn't work
#' @param expr
#' @param err.msg
#' @export
try_log <- function(expr, err.msg = "") {
  out <- try(expr, silent = TRUE)  
  if (class(out) == "try-error") {
    log_error("Fatal error running: %s. %s", deparse(substitute(expr)), err.msg)
  } else {
    if (!is.null(out))
      return(out)
  }
}

#' developer error logging
#' @inheritParams log_message
#' @export
log_error_dev <- function(msg, ...) {
  log_message(msg, ..., type = "FATAL ERROR")
}
