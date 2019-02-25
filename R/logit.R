#' Logit of x for 0 <= x <= 1. 
#'
#' @param p numeric between zero and one, possibly a vector
#'
#' @return logit of p
#' @export
#'
#' @examples logit(0.2)
logit <- function(p) {
    -log(1/p - 1)
}