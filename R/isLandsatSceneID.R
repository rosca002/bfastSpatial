#' @title Check if a String is a Landsat Scene ID
#' 
#' @description Parses a vector of characters to check if they conform to Landsat naming convention
#'  
#' @param x Character or object of class RasterBrick or RasterStack
#' 
#' @return \code{TRUE} if all elements of \code{x} match Landsat scene ID criteria, or \code{FALSE} otherwise.
#' 
#' @author Ben DeVries, Loic Dutrieux
#' 

.isLandsatSceneID <- function(x){
        if(is.character(x)) {
            all(grepl(pattern='L(C|O|T|E|M)0?[1-8](.*T[1-2]|\\d{13})', x))
        } else if(inherits(x, 'RasterStackBrick')) {
            all(grepl(pattern='L(C|O|T|E|M)0?[1-8](.*T[1-2]|\\d{13})', x=names(x)))
        }
}
