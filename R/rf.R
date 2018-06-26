#file for randomForest

#' rf models
#'
#' rf is a method used to fit random forest models using the randomForest package.
#'
#'
#' @param X numerical matrix of dimension n by p regressors
#' @param Y numerical vector of size n containing the response to predict.
#' @param Ylabel a character value containing the name of the Y varaible
#'
#' @return An object with S3 class "rf" with the following components:
#'
#' \item{randomForest list}{ contain the fitted model.
#' For more detail see \code{\link{randomForest::randomForest}}
#'
#' \item{coef_correction}{coefficient of linear correction for the randomForest model}
#'
#' \item{Xlabels}{a character vector containing the p regessor names}
#'
#' \item{Ylabel}{a character value containing the response variable name}
#'
#' \item{x_train}{the training matrix X of dimension \code{p} times \code{n}}
#'
#' \item{y_train}{the training response vector Y of length \code{n}}
#'
#' @seealso  \code{\link{predict.rf}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- rf(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' predict(model, newdata = indicateurs[1,])
#'
#'
#' @export


rf <- function(Y, X, ntree = 500, mtry, Ylabel = NULL){

  if (missing(mtry)) {
    model <- tuneRFalex(X, Y, trace = FALSE, plot = FALSE, doBest=TRUE)
  }else{
    model <- randomForest::randomForest(Y~., data = X, mtree = mtry)
  }

  model$Xlabels <- colnames(X)
  model$Ylabel <- Ylabel
  model$y_train <- Y
  model$x_train <- X
  model$coef_correction  <- lm(Y~predict(model, X))$coefficients

  class(model) <- 'rf'

  model
}

#' Predict method for rf model fits
#'
#' Predicted values based on rf model object.
#'
#'
#' @param model 'rf' class object
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
#' @seealso  \code{\link{rf}}, \code{\link{underModel.rf}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- rf(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' predict(model, newdata = indicateurs[1,])
#' predict(model, newdata = indicateurs[1,], predict.all = TRUE, R = 10)
#'
#' @export


predict.rf <- function(model, newdata, predict.all=FALSE, R = 100, ...){

  library(randomForest)
  model_cl_orig <- model
  class(model_cl_orig) <- 'randomForest'
  res <- predict(model_cl_orig, as.data.frame(newdata))
  res <- model$coef_correction[1] + model$coef_correction[2] * res
  res <- as.vector(res)

  if (!predict.all){
    return(res)
  }else {
    if (is.null(model$all_models)){
      model$all_models <- underModels.rf(model, B = R)
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

#' Generate undermodels to 'rf' class object
#'
#' Allow to generate undermodels by bootstraping the dataset used for training.
#'
#'
#' @param model 'rf' class object
#' @param B numeric value which is the number of undermodels generate
#' @param ... further arguments passed to or from other methods.
#'
#' @return a list of 'rf' models
#'
#' @seealso  \code{\link{rf}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- rf(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' model$undermodels <- undermodels.rf(model, B = 10)
#'
#' @export


underModels.rf <- function(model, B = 100, ...){

  all_models <- list()
  for (b in 1:B){
    train <- base::sample(1:length(model$y_train), replace = TRUE)
    Y_train <- model$y_train[train]
    X_train <- model$x_train[train,]

    model_b <- rf(Y = Y_train, X = X_train, Ylabel = model$Ylabel)
    all_models[[b]] <- model_b
  }

  all_models
}


#little correction of a rondomForest package function : tuneRF
tuneRFalex <- function (x, y, mtryStart = if (is.factor(y)) floor(sqrt(ncol(x))) else floor(ncol(x)/3),
                        ntreeTry = 50, stepFactor = 2, improve = 0.05, trace = TRUE,
                        plot = TRUE, doBest = FALSE, ...)
{
  if (improve < 0)
    stop("improve must be non-negative.")
  classRF <- is.factor(y)
  errorOld <- if (classRF) {
    randomForest::randomForest(x, y, mtry = mtryStart, ntree = ntreeTry,
                 keep.forest = FALSE, ...)$err.rate[ntreeTry, 1]
  }
  else {
    randomForest::randomForest(x, y, mtry = mtryStart, ntree = ntreeTry,
                 keep.forest = FALSE, ...)$mse[ntreeTry]
  }
  if (errorOld < 0)
    stop("Initial setting gave 0 error and no room for improvement.")
  if (trace) {
    cat("mtry =", mtryStart, " OOB error =", if (classRF)
      paste(100 * round(errorOld, 4), "%", sep = "")
      else errorOld, "\n")
  }
  oobError <- list()
  oobError[[1]] <- errorOld
  names(oobError)[1] <- mtryStart
  for (direction in c("left", "right")) {
    if (trace)
      cat("Searching", direction, "...\n")
    Improve <- 1.1 * improve
    mtryBest <- mtryStart
    mtryCur <- mtryStart
    while (Improve >= improve) {
      mtryOld <- mtryCur
      mtryCur <- if (direction == "left") {
        max(1, ceiling(mtryCur/stepFactor))
      }
      else {
        min(ncol(x), floor(mtryCur * stepFactor))
      }
      if (mtryCur == mtryOld)
        break
      errorCur <- if (classRF) {
        randomForest::randomForest(x, y, mtry = mtryCur, ntree = ntreeTry,
                     keep.forest = FALSE, ...)$err.rate[ntreeTry,
                                                        "OOB"]
      }
      else {
        randomForest::randomForest(x, y, mtry = mtryCur, ntree = ntreeTry,
                     keep.forest = FALSE, ...)$mse[ntreeTry]
      }
      if (trace) {
        cat("mtry =", mtryCur, "\tOOB error =", if (classRF)
          paste(100 * round(errorCur, 4), "%", sep = "")
          else errorCur, "\n")
      }
      oobError[[as.character(mtryCur)]] <- errorCur
      Improve <- 1 - errorCur/errorOld
      if (trace) cat(Improve, improve, "\n") #correction to avoid function to print this only if trace = False
      if (Improve > improve) {
        errorOld <- errorCur
        mtryBest <- mtryCur
      }
    }
  }
  mtry <- sort(as.numeric(names(oobError)))
  res <- unlist(oobError[as.character(mtry)])
  res <- cbind(mtry = mtry, OOBError = res)
  if (plot) {
    plot(res, xlab = expression(m[try]), ylab = "OOB Error",
         type = "o", log = "x", xaxt = "n")
    axis(1, at = res[, "mtry"])
  }
  if (doBest)
    res <- randomForest::randomForest(x, y, mtry = res[which.min(res[,
                                                       2]), 1], ...)
  res
}
