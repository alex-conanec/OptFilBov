#' Simulation
#'
#'
#' @param data the data frame of matrix use to train the models
#' @param models list of the models of class \code{\link{reg_lm}, \code{\link{ridge},
#' \code{\link{rf}, \code{\link{plsr}, \code{\link{sir}
#' @param n number of virtual individu to predict
#' @param borne
#' @param list_index
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


simulation <- function(data, models, n, index, borne, list_index, seed = 123){

  #create a 2n*p matrix with contain rondom value for index columns between +/- borne
  set.seed(seed)
  p <- length(models)
  col_names <- c()
  for (i in 1:p) col_names <- c(col_names, models[[i]]$Ylabel)
  k <- (1:p)[-index]
  res <- matrix(0, ncol = p, nrow = 100*n)
  colnames(res) <- col_names

  i <- 1
  for (j in index){

    res[, j] <- runif(100*n, min = -borne[i], max = borne[i])
    i <- i + 1
  }


  #delete impossible combination
  ki <- c()
  for (k in 1:length(list_index)) ki <- c(ki, length(list_index[[k]]))
  clusters_correlation <- lm_link(data, PI = length(list_index), ki = ki)
  to_delete <-c()

  i <- 1
  while ((i - length(unique(to_delete))) < n ){
    for (k in index){

      IP <- predict_cluster_interval(list_models_lm_i = clusters_correlation[[k]],
                                     newdata = as.data.frame(t(res[i, index])))

      if (! (res[i,k] > IP$min & res[i,k] < IP$max)){
        to_delete <- c(to_delete, i)
      }
    }
    i <- i + 1
  }

  if (i < 100*n) res <- res[1:i, ]
  if (length(to_delete) > 0) res <- res[-unique(to_delete), ]


  #generate the other columns with the models
  for (i in 1:nrow(res)){

    pred_cow <- predict_cow(cow_simulated = as.data.frame(t(res[i, ])), models = models, list_index = index,
                            list_value = res[i, index], B=5, R=10)

    for (j in (1:ncol(res))[-index]){

      res[i, j] <- pred_cow[1, j]

    }
  }

  as.data.frame(res)
}


#predict_cluster_interval ----
predict_cluster_interval <- function(list_models_lm_i, newdata){

  interval_lwr <- c()
  interval_upr <- c()


  for (i in 1:length(list_models_lm_i)){

    if (! is.null(list_models_lm_i[[i]])){
      interval_lwr <- c(interval_lwr, predict(list_models_lm_i[[i]], newdata = newdata, interval = 'prediction')[,2])
      interval_upr <- c(interval_upr, predict(list_models_lm_i[[i]], newdata = newdata, interval = 'prediction')[,3])
    }
  }

  list(min = max(interval_lwr), max = min(interval_upr))

}


#lm_link ----
lm_link <- function(X, PI, ki){

  res = list()
  col_offset <- 0

  for (j in 1:PI){
    if (j > 1) col_offset <- col_offset + ki[j-1]

    for (k in 1:ki[j]){
      res[[k + col_offset]] <- list()

      for (i in (1:ki[j])[!(1:ki[j]) == k]){

        formula <- as.formula(paste(colnames(X)[k + col_offset], '~', colnames(X)[i + col_offset]))
        res[[k + col_offset]][[i]] <- lm(formula, data = as.data.frame(X))
      }
    }
  }

  res
}





