loadDigestDataFromXL <- function(regionName, xlsPath, sheets, labelCol="B", positionCol="C", dataCol="D") {
  # Loads digest data from Excel. The data for fire frequency samples
  # are in separate worksheets where the sheet names are the sample code
  # (e.g. K1F1 for Kulnura, fire freq = 1, rep = 1).
  # The data are formatted in blocks with header rows and blank lines
  # so this script has to match things up.
  #
  # The value returned is a single data.frame with cols for sample label
  # and data value (both as character).
  
  require(xlsx)
  require(stringr)
  
  # colname for digest carbon
  valueName <- "digestC"
  
  gc()
  
  wb <- loadWorkbook(xlsPath)
  wss <- getSheets(wb)[sheets]
  
  wsNames <- str_trim(names(wss))
  
  getColValues <- function(ws, col, rowIndices) {
    if (missing(rowIndices)) 
      rows <- getRows(ws)
    else
      rows <- getRows(ws, rowIndices)
    
    sapply(getCells(rows, col), getCellValue)
  }
  
  cellNamesToRowIndex <- function(cellNames) {
    # names have the form rownum.colnum
    rows <- sapply( str_split(cellNames, "\\."), function(parts) parts[1] )
    as.integer(rows)
  }

  dat <- NULL
  for (isheet in 1:length(wss)) {
    
    labelCol <- xlColLabelToIndex(labelCol)
    labels <- getColValues(wss[[isheet]], labelCol)
    
    # The rows that we want are identified by having a label
    # identical to the worksheet name (e.g. "K1F1")
    ii <- labels == wsNames[isheet]
    
    # We need to get the excel row numbers from the label vector names
    # because of some cols having or not having header data
    ii.rows <- cellNamesToRowIndex( names(ii)[ii] )
    
    positionCol <- xlColLabelToIndex(positionCol)
    positionVals <- getColValues(wss[[isheet]], positionCol, ii.rows)
    
    dataCol <- xlColLabelToIndex(dataCol)
    dataVals <- getColValues(wss[[isheet]], dataCol, ii.rows)
    
    dat <- rbind(dat, data.frame(label=labels[ii], position=positionVals, x=dataVals, 
                                 stringsAsFactors=FALSE))
    
  }
  
  dat <- mutate_each(dat, funs( str_trim(as.character(.)) ))
  
  colnames(dat) <- c("label", "position", valueName)
  
  dat[, valueName] <- as.numeric(dat[, valueName])
  
  dat$region <- regionName
  
  dat <- mutate(dat, firefreq = factor(as.integer(str_sub(label, 2, 2))) )
  
  dat <- mutate(dat, bark = factor(str_sub(position, 1, 1), levels=c("O", "R", "S")))
  
  dat$depth <- NA_character_
  
  ii <- str_detect(dat$position, "[tT]\\s*5")
  dat$depth[ii] <- "a_to5"
  
  ii <- str_detect(dat$position, "5\\s*\\-\\s*13")
  dat$depth[ii] <- "b_5to13"
  
  ii <- str_detect(dat$position, ">\\s*13")
  dat$depth[ii] <- "c_gt13"
  
  dat$depth <- factor(dat$depth)  
  
  select_(dat, "region", "label", "position", "depth", "bark", "firefreq", valueName)
}
