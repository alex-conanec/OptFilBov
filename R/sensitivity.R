sobol_sensitivity <- function(model, ordre = 1, n = 1000, B = 100, seed = 123){

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
  sa <- sensitivity::sobol(model = model, X1 = x1, X2 = x2, order = ordre, nboot = B)

  sa$S

}
