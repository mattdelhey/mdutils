
#' strSplitOnFirstToken
#' helper function for read_ini
.strSplitOnFirstToken <- function(s, token="=") {
	pos <- which(strsplit(s, '')[[1]]==token)[1]
	if (is.na(pos)) {
		return(c(trim(s), NA)) 
	} else {
		first <- substr(s, 1, (pos-1))
		second <- substr(s, (pos+1), .nchar(s))
		return(trim(c(first, second)))
	}
}


#' strSplitOnLastToken
#' helper function for read_ini
.strSplitOnLastToken <- function(s, token="=") {
	# not used here
	pos <- unlist(strsplit(s, ''), use.names = FALSE)
	pos <- max(which(pos==token))
	if (!is.finite(pos)) {
		return(c(s, NA)) 
	} else {
		first <- substr(s, 1, (pos-1))
		second <- substr(s, (pos+1), .nchar(s))
		return(trim(c(first, second)))
	}
}

#' nchar
#' helper function for read_ini
.nchar <- function(x) {
	x[is.na(x)] <- ''
	sapply(strsplit(x, NULL), length)
}

#' read ini file
#' stolen from \textbf{raster} package
#' @param filename Character. Filename of the .ini file
#' @param token Character. The character that separates the "name" (variable name) from the "value"
#' @param commenttoken Character. This token and everything that follows on the same line is considered a 'comment' that is not for machine consumption and is ignored in processing
#' @param aslist Logical. Should the values be returned as a list
#' @param case Optional. Function that operates on the text, such as toupper or tolower
#' This function allows for using inistrings that have "=" as part of a value (but the token cannot be part of the 'name' of a variable!). Sections can be missing.
#' Authors: Robert J. Hijmans 
#' Date : October 2008
#' Version 0.9
#' Licence GPL v3
#' @return A n*3 matrix of characters with columns: section, name, value; or a list if aslist=TRUE.
read_ini <- function(filename, token='=', commenttoken=';', aslist=FALSE, case) {

  stopifnot(file.exists(filename))
	
	Lines <- trim(readLines(filename,  warn = FALSE))
	
	ini <- lapply(Lines, function(s){ .strSplitOnFirstToken(s, token=commenttoken) } ) 
	Lines <- matrix(unlist(ini), ncol=2, byrow=TRUE)[,1]
	ini <- lapply(Lines, function(s){ .strSplitOnFirstToken(s, token=token) }) 
	
 	ini <- matrix(unlist(ini), ncol=2, byrow=TRUE)
	ini <- ini[ ini[,1] != "", , drop=FALSE]

	ns <- length(which(is.na(ini[,2])))
	if (ns > 0) {
		sections <- c(which(is.na(ini[,2])), length(ini[,2]))

                                        # here I should check whether the section text is enclosed in [ ]. If not, it is junk text that should be removed, rather than used as a section
		ini <- cbind("", ini)
		for (i in 1:(length(sections)-1)) {
			ini[sections[i]:(sections[i+1]), 1] <- ini[sections[i],2]
		}	
		ini[,1] <- gsub("\\[", "", ini[,1])
		ini[,1] <- gsub("\\]", "", ini[,1])
		sections <- sections[1:(length(sections)-1)]
		ini <- ini[-sections,]
	} else {
		ini <- cbind("", ini)	
	}
  
	if (!missing(case)) {
		ini <- case(ini)
	}	
  
	colnames(ini) <- c("section", "name", "value")
	
	if (aslist) {

		iniToList <- function(ini) {
			un <- unique(ini[,1])
			LST <- list()
			for (i in 1:length(un)) {
				sel <- ini[ini[,1] == un[i], 2:3, drop=FALSE]
				lst <- as.list(sel[,2])
				names(lst) <- sel[,1]
				LST[[i]] <- lst
			}
			names(LST) <- un
			return(LST)
		}

		ini <- iniToList(ini)
	}
	
	return(ini)
}


# Author: Robert J. Hijmans
# Date : December 2009
# Version 1.0
# Licence GPL v3

if (!isGeneric("trim")) {
	setGeneric("trim", function(x, ...)
		standardGeneric("trim"))
}	


setMethod('trim', signature(x='character'), 
	function(x, ...) {
		gsub("^\\s+|\\s+$", "", x)
	}
)

setMethod('trim', signature(x='data.frame'), 
	function(x, ...) {
		for (i in 1:ncol(x)) {
			if (class(x[,i]) == 'character') {
				x[,i] <- trim(x[,i])
			} else if (class(x[,i]) == 'factor') {
				x[,i] <- as.factor(trim(as.character(x[,i])))
			}	
		}
		return(x)
	}
)


setMethod('trim', signature(x='matrix'), 
	function(x, ...) {
		if (is.character(x)) {
			x[] = trim(as.vector(x))
		} else {
			rows <- rowSums(is.na(x))
			cols <- colSums(is.na(x))
			rows <- which(rows != ncol(x))
			cols <- which(cols != nrow(x))
			if (length(rows)==0) {
				x <- matrix(ncol=0, nrow=0)
			} else {
				x <- x[min(rows):max(rows), min(cols):max(cols), drop=FALSE]
			}
		}
		return(x)
	}
)


# June 2013, modification by Mike Sumner, added argument "value"

setMethod('trim', signature(x='Raster'), 
function(x, padding=0, values=NA, filename='', ...) {


	filename <- trim(filename)

	nr <- nrow(x)
	nc <- ncol(x)
	nrl <- nr * nlayers(x)
	ncl <- nc * nlayers(x)
	
	cnt <- 0
	for (r in 1:nr) {
		v <- getValues(x, r)
		if (sum(v %in% values) < ncl) {
			break 
		}
		cnt <- cnt + 1
	}
	if ( cnt == nr) { stop('only NA values found') }
	firstrow <- min(max(r-padding, 1), nr)
	
	for (r in nr:1) {
		v <- getValues(x, r)
		if (sum(v %in% values) < ncl) { break }
	}
	lastrow <- max(min(r+padding, nr), 1)
	
	if (lastrow < firstrow) { 
		tmp <- firstrow
		firstrow <- lastrow
		lastrow <- tmp
	}
	
	for (c in 1:nc) {
		v <- getValuesBlock(x, 1 ,nrow(x), c, 1)
		if (sum(v %in% values) < nrl) { break }
	}
	firstcol <- min(max(c-padding, 1), nc) 
	
	for (c in nc:1) {
		v <- getValuesBlock(x, 1 ,nrow(x), c, 1)
		if (sum(v %in% values) < nrl) { break }
	}
	lastcol <- max(min(c+padding, nc), 1)
	
	if (lastcol < firstcol) { 
		tmp <- firstcol
		firstcol <- lastcol
		lastcol <- tmp
	}
	
	xr <- xres(x)
	yr <- yres(x)
	e <- extent(xFromCol(x, firstcol)-0.5*xr, xFromCol(x, lastcol)+0.5*xr, yFromRow(x, lastrow)-0.5*yr, yFromRow(x, firstrow)+0.5*yr)
	
	return( crop(x, e, filename=filename, ...) )
}
)

