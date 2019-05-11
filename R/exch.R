#' Generate a normal distributed sample X with dimension n *p with mean 0
#' and an exchangable covariance matrix where a_{ij} = rho^{i-j}
#'
#'
#' @param rho  auto correlation structure
#' @param P  dimension of the data
#' @param N  sample size of the data
#'
#' @return X generated data
#'
#' @examples
#'  Exch(rho = 0.2, P = 10, N = 300)
#'
#' @export
#'

Exch <- function(rho, P, N){

  SIGMA <-  matrix(rep(rho,P^2), P, P)

  diag(SIGMA) <- 1

  X <- mvtnorm::rmvnorm(N,mean = rep(0,P),sigma = SIGMA)

  return(X)
}
