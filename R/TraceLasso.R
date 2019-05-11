#' fit a model with given X and y
#'  with trace lasso method
#'
#'
#' @param X  auto correlation structure
#' @param y  dimension of the data
#' @param path abslute path where the python file is stored
#' @param logistic indicator for using logistic or not
#'
#' @return coefficients generated
#'
#' @examples
#'  X = matrix(rnorm(60*50), ncol = 60, nrow = 50)
#'  y = rnorm(50)
#'  lmcoef = TraceLasso(X, y, logistic = TRUE)
#'
#' @export
#'


TraceLasso = function(X,y,
                      path = "/Users/w/Desktop/UPenn\ Course/2019\ spring/computing/pslassoR/R/",
                      logistic = TRUE){

  reticulate::use_python("/usr/local/bin/python2", required = FALSE)

  models <- reticulate::import_from_path("models", path = path)

  if (nrow(X) != length(y)) {
    stop("X and y is need to be same length ")
  }

  model = models$PrecisionLasso()

  model$setLogisticFlag(logistic)

  model$setLambda(1)

  model$setLearningRate(1e-6)

  model$setGamma(1) # Calculate gamma

  model$fit(X,y) # Calculate gamma

  H = model$getBeta()

  return(H)
}
