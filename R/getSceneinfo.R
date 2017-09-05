
#' @title Retrieve Landsat info from filenames
#' 
#' @description Parses through typical Landsat filenames and retrieves information on sensor and acquisition date. Vectorized over \code{sourcefile}.
#' 
#' @param sourcefile Character. Filename of a landsat layer or dataset.
#' @param ... Additional arguments to pass to \code{\link{write.csv}}.
#' 
#' @author Ben DeVries and Loic Dutrieux
#' 
#' @return a \code{data.frame} with parsed scene information from Landsat scene names
#' 
#' @import stringr
#' 
#' @examples 
#' getSceneinfo(c('ndvi.LC82300702014234.tar.gz', 'ndvi.LT52300702008234.tif'))
#' 
#' # Load tura
#' data(tura)
#' getSceneinfo(names(tura))
#' 
#' @export


getSceneinfo <- function(sourcefile, ...)
{
  # for the sake of convenience, sourcefile can be either a character vector of scene names (or subfolders) or the original .tar.gz or .tar files
  # this will check which it is and format accordingly
  if(!all(grepl(pattern='L(C|O|T|E|M)0?[1-8](.*T[1-2]|\\d{13})', x=sourcefile)))
      warning('Some of the characters provided do not contain recognized Landsat5/7/8 scene ID')
    
    
  sourcefile <- str_extract(sourcefile, 'L(C|O|T|E|M)0?[1-8](.*T[1-2]|\\d{13})') 
  
  
  # dates in LS naming system are formatted with year and julian day as one number - "%Y%j" (e.g. 2001036 = 2001-02-05)
  # reformat date as "%Y-%m-%d" (ie. yyyy-mm-dd)
  if grepl(pattern = 'L(C|O|T|E|M)0[1-8]', sourcefile){
    dates <- as.Date(substr(sourcefile, 18, 25), format="%Y%m%d")
    # extract path, row
    path <- as.numeric(substr(sourcefile, 11, 13))
    row <- as.numeric(substr(sourcefile, 14, 16))
  }
  else
    {
    dates <- as.Date(substr(sourcefile, 10, 16), format="%Y%j")
    # extract path, row
    path <- as.numeric(substr(sourcefile, 4, 6))
    row <- as.numeric(substr(sourcefile, 7, 9))
    }

  
  # identify the sensor
  sensor <- as.character(mapply(sourcefile, dates, FUN=function(x,y){
    sen <- grep(pattern = 'L(C|O|T|E|M)0?[1-8]', x, value = TRUE)
    if(is.na(sen)) NA 
    else if(grepl(pattern = 'LE0?7', sen) & y <= "2003-03-31")
      "ETM+ SLC-on"
    else if(grepl(pattern = 'LE0?7', sen) & y > "2003-03-31")
      "ETM+ SLC-off"
    else if(grepl(pattern = 'LT0?5', sen) | grepl(pattern = 'LT0?4', sen)) 
      "TM" 
    else if(grepl(pattern = 'LC0?8', sen))
      "OLI"      
  }))
  
  # throw all attributes into a data.frame
  info <- data.frame(sensor = sensor, path = path, row = row, date = dates)
  sourcefile[is.na(sourcefile)] <- 'Not recognised'
  row.names(info) <- sourcefile
  
  # optional: print to .csv for future reference
  if(hasArg(file)) 
    write.csv(info, ...)
  
  return(info)
}
