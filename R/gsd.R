#' Compute the geometric standard deviation of an object
#'
#' @param x an R object for which the geometric standard deviation is to be computed
#' @param zn.rm remove zeros and negatives
#' @param na.rm logical indicating whether \code{NA} values should be stripped
#'
#' @return geometric standard deviation
#' @export
#'
#' @examples gsd(seq(1,100,by=10))
gsd = function(x, zn.rm = FALSE, na.rm = FALSE) {
    
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
    exp(log(stats::sd(.x)))
}