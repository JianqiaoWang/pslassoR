#' fit a model with given X and y with
#' precision lasso method
#'
#'
#' @param X  auto correlation structure
#' @param y  dimension of the data
#' @param logistic indicator for doing regression on binary response
#' @param method choose the method: lasso, ridge or elasticnet
#' @return coefficients generated
#'
#' @examples
#'  X = matrix(rnorm(60*50), ncol = 60, nrow = 50)
#'  y = rnorm(50)
#'  lmcoef = glmreg(X, y, logistic = FALSE, method = "lasso")
#' @export
#'


glmreg = function(X, y, logistic = TRUE, method = c("lasso", "ridge", "elasticnet")){

  alpha = switch(method,
                 lasso = 1,
                 ridge = 0,
                 elasticnet = 0.5)

  if(logistic){
    dist = "binomial"
  }else{
    dist = "gaussian"
  }

  if (nrow(X) != length(y)) {
    stop("X and y is need to be same length ")
  }

  fit = glmnet::cv.glmnet(X, y, family = dist, alpha = alpha, nlambda = 100, intercept=F)

  small.lambda.betas <- coef(fit, s = "lambda.min")

  return(small.lambda.betas[-1])
}
