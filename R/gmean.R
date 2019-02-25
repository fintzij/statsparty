#' Compute the geometric mean of an object
#'
#' @param x an R object for which the geometric mean is to be computed
#' @param zn.rm remove zeros and negatives
#' @param na.rm logical indicating whether \code{NA} values should be stripped
#'
#' @return geometric mean
#' @export
#'
#' @examples gmean(seq(1,100,by=10))
gmean = function(x, zn.rm = FALSE, na.rm = FALSE) {
    
    .x = if(!zn.rm & !na.rm) {
        x
    } else if(zn.rm & !na.rm) {
        x[x>0]
    } else if(!zn.rm & na.rm) {
        x[!is.na(x)]
    } else if(zn.rm & na.rm) {
        x[x>0 & !is.na(x)]
    }
    
    # return geometric mean
    exp(log(mean(.x)))
}