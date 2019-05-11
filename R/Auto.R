#' Generate a normal distributed sample X with dimension n *p with mean 0
#' and an autocorrelation covariance matrix where a_{ij} = rho^{i-j}
#'
#'
#' @param rho  auto correlation structure
#' @param P  dimension of the data
#' @param N  sample size of the data
#'
#' @return X generated data
#'
#' @examples
#'  Auto(rho = 0.2, P = 10, N = 300)
#'
#' @export
#'

Auto <- function(rho, P, N){

  stopifnot(is.numeric(rho) & is.numeric(P) & is.numeric(N))

  SIGMA <-  rho^abs(outer(1:P, 1:P, "-"))

  X <- mvtnorm::rmvnorm(N, mean = rep(0,P),sigma = SIGMA)

  return(X)

}
