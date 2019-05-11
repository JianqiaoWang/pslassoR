#' Generate a sparse vector whose non-zero elements following
#' U(0,1) distribution.
#'
#'
#' @param K  number of non-zero elements
#' @param P  length of the vector
#'
#' @return A sparse vector
#'
#' @examples
#'  beta.coef(K = 1, P = 10)
#'
#' @export
#'

beta.coef = function(K, P){

  coef = rep(0,P)

  coef[sample(1:P,K)] = runif(K)

  return(coef)

}

