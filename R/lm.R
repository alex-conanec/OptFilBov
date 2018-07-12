#file for reg_lm model

#' reg_lm models
#'
#' reg_lm is a method used to fit linear models using the lm package.
#'
#'
#' @param X numerical matrix of dimension n by p regressors
#' @param Y numerical vector of size n containing the response to predict.
#' @param Ylabel a character value containing the name of the Y varaible
#'
#' @return An object with S3 class "reg_lm" with the following components:
#'
#' \item{lm list}{ lm list containing the fitted model.
#' For more detail see \code{\link{stats::lm}}
#'
#' \item{Xlabels}{a character vector containing the p regessor names}
#'
#' \item{Ylabel}{a character value containing the response variable name}
#'
#' \item{x_train}{the training matrix X of dimension \code{p} times \code{n}}
#'
#' \item{y_train}{the training response vector Y of length \code{n}}
#'
#' @seealso  \code{\link{predict.reg_lm}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- reg_lm(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' predict(model, newdata = indicateurs[1,])
#'
#'
#' @export


reg_lm <- function(X, Y, Ylabel=NULL){

  model<- lm(Y~., data = as.data.frame(X))
  model$Xlabels <- colnames(X)
  model$Ylabel <- Ylabel
  model$y_train <- Y
  model$x_train <- X
  class(model) <- 'reg_lm'
  model
}

#predict reg_lm
predict.reg_lm <- function(model, newdata, predict.all = FALSE, R = 100, interval = FALSE, interval_method = NULL, ...){

  model_cl_orig <- model
  class(model_cl_orig) <- 'lm'
  res <- predict(model_cl_orig, as.data.frame(newdata), ...)
  res <- as.vector(res)

  if (!predict.all){
    if (!interval) return(res) else{
      return(predict(model_cl_orig, newdata, interval = "prediction")[-1])
    }

  }else {
    if (is.null(model$all_models)){
      model$all_models <- underModels.reg_lm(model, B = R)
    }

    res_r <- c()
    R <- min(R, length(model$all_models))
    for (r in 1:R){
      res_r <- c(res_r, predict(model = model$all_models[[r]],
                                newdata = newdata,
                                predict.all = FALSE))
    }
    list(aggregate = res,
        individual = matrix(res_r, byrow = TRUE, ncol = nrow(newdata), nrow = R))

  }
}



#' Predict method for reg_lm model fits
#'
#' Predicted values based on reg_lm model object.
#'
#'
#' @param model 'reg_lm' class object
#' @param newdata matrix containg the regressor of the model that you want to predict the response for.
#' @param predict.all logical value (FALSE by default).
#' If TRUE, a matrix of dimension \code{R} by \code{nrow(neawdata)} is return,
#' containing several predictions from undermodels generate by \code{\link{underModel.rf}}
#' @param R numeric value which set the number of under-prediction made when predict.all=TRUE
#' @param ... further arguments passed to or from other methods.
#'
#' @return Either a numeric value if predict.all=FALSE
#' or a list containing the following component if predict.all=TRUE:
#'
#' \item{aggregate}{a numeric value or matrix which is the prediction from the model
#'
#' \item{individual}{a matrix of dimension \code{R} by \code{nrow(neawdata)}
#' containing the R prediction for the n newdata passed throught}
#'
#' @seealso  \code{\link{reg_lm}}, \code{\link{underModel.reg_lm}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- reg_lm(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' predict(model, newdata = indicateurs[1,])
#' predict(model, newdata = indicateurs[1,], predict.all = TRUE, R = 10)
#'
#' @export


underModels.reg_lm <- function(model, B = 100, ...){

  all_models <- list()
  for (b in 1:B){
    train <- base::sample(1:length(model$y_train), replace = TRUE)
    Y_train <- model$y_train[train]
    X_train <- model$x_train[train,]

    model_b <- reg_lm(X = X_train, Y = Y_train, Ylabel = model$Ylabel)
    all_models[[b]] <- model_b
  }

  all_models
}


#summary
summary.reg_lm <- function(object, ...){

  class(object) <- 'lm'
  summary(object)

}
