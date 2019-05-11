
#' Caculate AUC
#'
#' @param T.Beta  True label
#' @param Beta  estimated coefficient
#'
#' @return X generated data
#'
#' @examples
#'  AUC(T.Beta = c(1,0,1), Beta <- c(0.4,0.7,0.1))
#'
#'
#' @export
#'



AUC = function(T.Beta, Beta){

  return(pROC::auc(T.Beta, Beta))

}
