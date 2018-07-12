#file for ridge model


#' Ridge models
#'
#' Ridge is a method used to fit linear models with constraint lambda using the glmnet package.
#'
#'
#' @param X numerical matrix of dimension n by p regressors
#' @param Y numerical vector of size n containing the response to predict.
#' @param Ylabel a character value containing the name of the Y varaible
#'
#' @return An object with S3 class "ridge" with the following components:
#'
#' \item{glmnet list}{ glmnet list containing the fitted model.
#' For more detail see \code{\link{glmnet::cv.glmnet}}
#'
#' \item{Xlabels}{a character vector containing the p regessor names}
#'
#' \item{Ylabel}{a character value containing the response variable name}
#'
#' \item{x_train}{the training matrix X of dimension \code{p} times \code{n}}
#'
#' \item{y_train}{the training response vector Y of length \code{n}}
#'
#' @seealso  \code{\link{predict.ridge}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- ridge(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' predict(model, newdata = indicateurs[1,])
#'
#'
#' @export


ridge <- function(X, Y, Ylabel = NULL){


  model <- glmnet::cv.glmnet(as.matrix(X), Y, family = "gaussian", alpha=0,
                             standardize = FALSE, nfolds = 10, grouped=FALSE)

  model$Xlabels <- colnames(X)
  model$Ylabel <- Ylabel
  model$y_train <- Y
  model$x_train <- X
  class(model) <- 'ridge'
  model
}


#' Predict method for ridge model fits
#'
#' Predicted values based on ridge model object.
#'
#'
#' @param model 'ridge' class object
#' @param newdata matrix containg the regressor of the model that you want to predict the response for.
#' @param predict.all logical value (FALSE by default).
#' If TRUE, a matrix of dimension \code{R} by \code{nrow(neawdata)} is return,
#' containing several predictions from undermodels generate by \code{\link{underModel.ridge}}
#' @param R numeric value which set the number of under-prediction made when predict.all=TRUE
#' @param interval a boolean value (FALSE by default) to return a interval of prediction
#' @param interval_method either 'sd' (by default) or 'qt'
#' @param ... further arguments passed to or from other methods.
#'
#' @return a numeric value if predict.all=FALSE and interval=FALSE.
#' if interval=TRUE it return a interval for each value to predict
#' or a list containing the following component if predict.all=TRUE:
#'
#' \item{aggregate}{a numeric value or matrix which is the prediction from the model
#'
#' \item{individual}{a matrix of dimension \code{R} by \code{nrow(neawdata)}
#' containing the R prediction for the n newdata passed throught}
#'
#'
#'
#' @seealso  \code{\link{ridge}}, \code{\link{underModel.ridge}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- ridge(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' predict(model, newdata = indicateurs[1,])
#' predict(model, newdata = indicateurs[1,], predict.all = TRUE, R = 10)
#'
#' @export


predict.ridge <- function(model, newdata, predict.all = FALSE, R = 100, interval = FALSE, interval_method = 'sd', ...){

  require(glmnet)
  if (class(newdata) == 'numeric') newdata <- as.data.frame(t(newdata))

  model_cl_orig <- model
  class(model_cl_orig) <- 'cv.glmnet'
  xnew <- as.matrix(newdata[ ,colnames(newdata) %in% model$Xlabels])
  res <- predict(model_cl_orig, xnew, s = "lambda.min", ...)
  res <- as.vector(res)

  if (!predict.all){
    if (!interval) return(res) else{

      if (is.null(model$all_models)){
        model$all_models <- underModels.rf(model, B = R)
      }

      pred_all <- predict(model, newdata, predict.all = TRUE, R = R)
      res <- apply(pred_all$individual, 2, function(x) {
        c(mean(x, na.rm = TRUE) + c(-1.96,1.96) * sd(x, na.rm = TRUE),
          quantile(x, c(0.025,0.975), na.rm = TRUE) )
      })
      if (interval_method == 'sd') {
        return(t(res)[,1:2]) } else if (interval_method == 'qt') {
          return(t(res)[,3:4])} else {
            stop("The \"interval_method\" have to be in c(\'sd\',\'qt\')", call. = FALSE)}
    }

  }else {
    if (is.null(model$all_models)){
      model$all_models <- underModels.ridge(model, B = 100)
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


#' Generate undermodels to 'ridge' class object
#'
#' Allow to generate undermodels by bootstraping the dataset used for training.
#'
#'
#' @param model 'ridge' class object
#' @param B numeric value which is the number of undermodels generate
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list of 'ridge' models
#'
#' @seealso  \code{\link{ridge}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- ridge(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' model$undermodels <- undermodels.ridge(model, B = 10)
#'
#' @export


underModels.ridge <- function(model, B = 100, ...){

  all_models <- list()
  for (b in 1:B){
    train <- base::sample(1:length(model$y_train), replace = TRUE)
    Y_train <- model$y_train[train]
    X_train <- model$x_train[train,]

    model_b <- ridge(X = X_train, Y = Y_train, Ylabel = model$Ylabel)
    all_models[[b]] <- model_b
  }

  all_models
}


#summary
summary.ridge <- function(object, ...){

  class(object) <- 'cv.glmnet'
  summary(object)

}
