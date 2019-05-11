#' fit a model with given X and y with
#' precision lasso method
#'
#'
#' @param X  design matrix
#' @param y  response vector
#' @param path abslute path where the python file is stored
#' @param logistic indicator for doing regression on binary response
#'
#' @return coefficients generated
#'
#' @examples
#'  X = matrix(rnorm(60*50), ncol = 60, nrow = 50)
#'  y = rnorm(50)
#'  lmcoef = PSlasso(X, y, logistic = TRUE)
#'
#' @export
#'


PSlasso <- function(X, y, path = "/Users/w/Desktop/UPenn\ Course/2019\ spring/computing/pslassoR/R/", logistic = TRUE){
 # if(!is.vector(y)){
 #    y = as.vector(y)
 #  }

  if (nrow(X) != length(y)) {
    stop("X and y is need to be same length ")
  }


  reticulate::use_python("/usr/local/bin/python2", required = FALSE)

  models <- reticulate::import_from_path("models", path = path)

  #models <- reticulate::py_run_file("models.py")

  model = models$PrecisionLasso()

  model$setLogisticFlag(logistic)

  model$setLambda(1)

  model$setLearningRate(1e-6)

  model$calculateGamma(X) # Calculate gamma

  model$fit(X,as.vector(y)) # Calculate gamma

  H = model$getBeta()

  return(H)
}
