#' Sobol sensitivity
#'
#' Sobol sensitivity calculation throught the package sensitivity
#'
#' @param model a model with a predict method
#' @param n size of the sample used by the Montecarlo method (1000 by default)
#' @param seed an integer to set the seed (123 by default)
#'
#' @return a matrix of the estimations of the Sobol sensitivity indices for each regressor
#'
#' @seealso  \code{\link{sensitivity::sobol}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' fit <- ridge(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' sobol_sensitivity(model = fit, order = 1, n = 1000, nboot = 100, seed = 123)
#'
#' @export


sobol_sensitivity <- function(model, n = 1000, seed = 123, ...){

  p <- length(model$Xlabels)
  x1 <- data.frame(matrix(nr = n, nc = p))
  x2 <- data.frame(matrix(nr = n, nc = p))
  colnames(x1) <- model$Xlabels
  colnames(x2) <- model$Xlabels

  set.seed(seed)
  for (i in 1:p){
    x1[, i] <- runif(n = n, min = -2, max = 2)
    x2[, i] <- runif(n = n, min = -2, max = 2)
  }

  # sensitivity analysis
  sa <- sensitivity::sobol(model = model, X1 = x1, X2 = x2, ...)

  sa$S

}

