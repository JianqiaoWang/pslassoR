#'Calculate the AUC for five variable selection method
#'
#' @param i  random seed
#' @param n  dimension of the data
#' @param K  Sparsity parameter
#' @param rho  parameter of covariance matrix
#' @param type  type of covariance matrix
#'
#' @return  calculated AUC for five methods
#'
#' @examples
#'  Simulation(i = 1, n = 100, K =10, rho = 0.2,
#'  type = "block")
#'
#' @export
#'

Simulation = function(i, n, K, rho, type, path){

  set.seed(i)

  n = n

  P = 500

  K = K

  rho = rho

  #K = k # K measures sparsity

  beta = beta.coef(K, P)

  if (type == "Auto"){

    X = Auto(rho, P = P, N = n)

  }

  if(type == "block"){

    X = Block(rho, P, N = n, bandwidth = 5)

  }

  if(type == "exch"){

    X = Exch(rho, P, N = n)

  }


  y = X %*% beta + rnorm(n)

  y = as.vector(y)

  T.Beta = (abs(beta) > 0)

  Beta.PSlasso = PSlasso(X, y, logistic = F)

  Beta.Tracelasso = TraceLasso(X, y, logistic = F)

  Beta.lasso = glmreg(X, y, logistic = F, method = "lasso")

  Beta.ridge = glmreg(X, y, logistic = F, method = "ridge")

  Beta.elsticnet = glmreg(X, y, logistic = F, method = "elasticnet")

  return(c(n, K, rho, K, AUC(T.Beta, Beta.PSlasso), AUC(T.Beta, Beta.Tracelasso), AUC(T.Beta, Beta.lasso), AUC(T.Beta, Beta.ridge), AUC(T.Beta, Beta.elsticnet)))

}

