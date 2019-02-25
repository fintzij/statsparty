#' Expit, i.e., the inverse of logit = log(x / (1-x))
#'
#' @param x numeric, possibly a vector
#'
#' @return expit(x)
#' @export
#'
#' @examples expit(-5)
expit <- function(x) {
    1/(1 + exp(-x))
}