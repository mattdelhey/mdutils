#' mnaually write formula
xl_manual2 <- function(rows, sheet, outcol) {
  n <- length(rows)
  out <- rep(NA, n)
  for (i in 1:n) {
    out[i] <- var_sub("weights!$C$2*TANH($O$%s/weights!$C$14) + weights!$C$3*TANH($P$%s/weights!$C$15) + weights!$C$4*TANH($Q$%s/weights!$C$16) + weights!$C$5*($AA$%s/5) + weights!$C$6*($AC$%s/5) + weights!$C$7*($AE$%s/5) + weights!$C$8*TANH($AB$%s/weights!$C$17) + weights!$C$9*TANH($AD$%s/weights!$C$18) + weights!$C$10*TANH($AF$%s/weights!$C$19) + weights!$C$11*$AG$%s + weights!$C$12*$AH$%s + weights!$C$13*$AI$%s", "%s"=rows[i])
    setCellFormula(wb, sheet=sheet, row=rows[i], col=outcol, formula=out[i])
  }
}

#' mnaually write formula
xl_manual <- function(rows, sheet, outcol) {
  n <- length(rows)
  out <- rep(NA, n)
  for (i in 1:n) {
    out[i] <- var_sub("WEIGHTS!$C%s*TANH($K%s/WEIGHTS!$C$14) + WEIGHTS!$C$3*TANH($L%s/WEIGHTS!$C$15) + WEIGHTS!$C$4*TANH($M%s/WEIGHTS!$C$16) + WEIGHTS!$C$5*($W%s/5) + WEIGHTS!$C$6*($Y%s/5) + WEIGHTS!$C$7*($AA%s/5) + WEIGHTS!$C$8*TANH($X%s/WEIGHTS!$C$17) + WEIGHTS!$C$9*TANH($Z%s/WEIGHTS!$C$18) + WEIGHTS!$C$10*TANH($AB%s/WEIGHTS!$C$19) + WEIGHTS!$C$11*$AC%s + WEIGHTS!$C$1%s*$AD%s + WEIGHTS!$C$13*$AE%s", "%s"=rows[i])
    setCellFormula(wb, sheet=sheet, row=rows[i], col=outcol, formula=out[i])
  }
}


#' Write data to region
#' @param wb XLConnect workbook
#' @param data data.frame
#' @param sheet 'character' sheet to write in
#' @param region 'character' excel region to name
#' @param where 'character' excel location formula
#' @param overwrite 'logical' overwrite region name?
#' @param header 'logical' write header?
#' @inheritParams XLConnect::writeNamedRegion
xl_write <- function(wb, data, sheet, region, where="$A$1", overwrite=TRUE, header=TRUE, rownames=NULL) {
  formula <- sprintf("%s!%s", sheet, where)
  createName(wb, name=region,  formula=formula, overwrite=overwrite)
  writeNamedRegion(wb, data, name=region, header=header, rownames=rownames)
}

#' write xl weights sheet
xl_weights <- function(wb, w, sheet) {
  createSheet(wb, name=sheet)
  xl_write(wb, data=w, sheet=sheet, region="weights", where="$A$1")
  ## Write sum of weights
  setCellFormula(wb, sheet=sheet, row=20, col=3, formula="SUM($C$2:$C$13)")  
}

#' write xl sheet
xl_sheet <- function(wb, data, sheet, outcol, where="$A$1") {
  ## Create new sheet in wb
  createSheet(wb, name=sheet)
  ## Write data (where="$E$1")
  xl_write(wb, data=data, sheet=sheet, region=sprintf("%s_tclist", sheet),  where=where)
  ## Write formulas
  exrows <- 2:(nrow(data)+1)
  xl_manual2(exrows, sheet=sheet, outcol=outcol)
  ## Calculate formulas
  setForceFormulaRecalculation(wb, sheet=sheet, value=TRUE)
  ## default to world
  setActiveSheet(wb, sheet="World")
  ## set autofilter (reference = aref("E1", dim(data)))
  setAutoFilter(wb, sheet=sheet, reference = aref("A1", dim(data)))
}


#' create a workbook
#' @param fname 'character' output file name
#' @param overwrite 'logical' overwrite if file exists?
#' @return XLConnect workbook
xl_create <- function(fname, overwrite=TRUE) {
  if (file.exists(fname)) {
    if (overwrite) {
      warning("file exists; overwriting")
      file.remove(fname)
    }
    if (!overwrite) {
      stop("file exists; not removing. failed to create workbook")
    }
  }
  loadWorkbook(fname, create=TRUE)
}



