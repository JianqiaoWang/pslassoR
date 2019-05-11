#' Generate a normal distributed sample X with dimension n *p with mean 0
#' and a blockdiagonal covariance matrix
#'
#'
#' @param rho  off-diagnoal elements value
#' @param P  dimension of the data
#' @param N  sample size of the data
#' @param bandwidth blok size
#'
#' @return X generated data
#'
#' @examples
#'  Block(rho = 0.2, P = 10, N = 300, bandwidth = 5)
#'
#' @export
#'

Block <- function(rho, P, N, bandwidth){

  A = matrix(rep(rho,(bandwidth)^2), bandwidth, bandwidth)

  diag(A) <- 1

  SIGMA <-  diag(P/bandwidth) %x% A

  X <- mvtnorm::rmvnorm(N,mean = rep(0,P),sigma = SIGMA)

  return(X)

}
