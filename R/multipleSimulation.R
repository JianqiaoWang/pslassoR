#' Multiple Simulation
#'
#' @param type.vec  covariance parameter vector
#' @param rho.vec  correlation parameter vector
#' @param n.vec  parameter vector of sample size
#' @param K.vec  parameter vector of sparsity
#' @param mccores   specify the cores number for parallel computing. Default is 1
#' @return  calculated AUC tables for different combination
#'
#' @examples
#'  multipleSimulation(type.vec = c("Auto"),
#'   rho.vec = c(0.2),
#'    n.vec =c(100),
#'     K.vec = c(20))
#'
#' @export
#'



multipleSimulation = function(type.vec, rho.vec, n.vec, K.vec, mccores = 1){

  result.all.auto <- vector()

  for(type in type.vec){
    for(rho in rho.vec){
      for(n in n.vec){
        for(K in K.vec){


          result = parallel::mclapply(1:10, Simulation, n = n, K = K, rho = rho, type = type, mc.cores = mccores)

          result.all.auto = rbind(result.all.auto,
                                  colMeans(do.call(rbind, result)))
        }
      }
    }
  }


  result.all.auto$type = rep(type.vec, each = (length(rho.vec)* length(n.vec) * length(K.vec)))

  return(result.all.auto)

}


