#file for sir method


#' sir models
#'
#' sir is a method used to fit sliced-inverse regression models
#'
#'
#' @param X numerical matrix of dimension n by p regressors
#' @param Y numerical vector of size n containing the response to predict.
#' @param H integer (by default 10) which set the number of slices
#' @param K integer (by default 1) which set the number of component taken. Only 1 is possible right now.
#' @param Ylabel a character value containing the name of the Y varaible
#'
#' @return An object with S3 class "sir" with the following components:
#'
#' \item{beta}{ }
#'
#' \item{index}{ }
#'
#' \item{H}{H parameter passed througth the function}
#'
#' \item{K}{K parameter passed througth the function }
#'
#' \item{hopt}{bandwidth value of the kernel tuned to smooth the points and predict new values.
#' For more explaination see \code{\link{cv_bandwidth}}}
#'
#' \item{Xlabels}{a character vector containing the p regessor names}
#'
#' \item{Ylabel}{a character value containing the response variable name}
#'
#' \item{x_train}{the training matrix X of dimension \code{p} times \code{n}}
#'
#' \item{y_train}{the training response vector Y of length \code{n}}
#'
#' @seealso  \code{\link{predict.sir}}
#'
#'
#' @examples
#' library(mfe)
#' data(indicateurs)
#' X <- indicateurs[, -c(1,2,3)]
#' Y <- indicateurs[,1]
#' model <- sir(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
#' predict(model, newdata = indicateurs[1,])
#'
#'
#' @export


sir <- function(Y, X, H = 10, K = 1, Ylabel = NULL){

  X <- as.matrix(X)
  beta <- edrGraphicalTools::edr(Y, X, H = H, K = K, method = "SIR-I")$matEDR[, 1, drop = FALSE]
  index <- X %*% beta
  hopt <- cv_bandwidth(index, Y, graph.CV=FALSE)$hopt
  model <- list(Xlabels = colnames(X),
                Ylabel = Ylabel,
                beta = beta,
                index = index,
                H = H,
                K = K,
                y_train = Y,
                x_train = X,
                hopt = hopt)
  class(model) <- 'sir'
  model
}


#predict.sir ----
predict.sir <- function(model, newdata, predict.all=FALSE, R = 100, interval = FALSE, interval_method = 'sd', ...){

  newdata <- as.data.frame(newdata)
  X <- as.matrix(newdata[, which(colnames(newdata) %in% model$Xlabels)])
  index <- X %*% model$beta
  res <- stats::ksmooth(model$index, model$y_train, kernel = "normal",
                        bandwidth = model$hopt, x.points = index)$y[rank(index)]

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

  }else{
    if (is.null(model$all_models)){
      model$all_models <- underModels.sir(model, B = R)
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



#addUnderModel ----
underModels.sir <- function(model, B = 100, ...){

  all_models <- list()
  for (b in 1:B){
    train <- base::sample(1:length(model$y_train), replace = TRUE)
    Y_train <- model$y_train[train]
    X_train <- model$x_train[train,]

    model_b <- sir(Y = Y_train, X = X_train, H = model$H, K = model$K, Ylabel = model$Ylabel)
    all_models[[b]] <- model_b
  }

  all_models
}


#plot.sir
plot.sir <- function(object, choice = 'eigenValue', ...){

  if (choice == 'eigenValue'){

    barplot(edrGraphicalTools::edr(object$y_train, object$x_train, H = object$H,
                                   K = object$K, method = "SIR-I")$eigvalEDR,
            main = 'eigen value of the parametric estimation')

  }else if (choice == 'smoothing'){

    plot(object$index, object$y_train, main = 'performance de l\'estimation non paramtrique du modele')
    lines(ksmooth(object$index,
                  object$y_train,
                  kernel = "normal",
                  bandwidth = object$hopt,
                  x.points = object$index),
          col = 'red')

  }

}


#cv_bandwidth ----
cv_bandwidth <- function(x, y, hmin=(max(x)-min(x))/20,
                         hmax=(max(x)-min(x))/2, nbh=25,
                         graph.CV=TRUE){
  n <- length(x)
  vecth <- seq(from = hmin, to = hmax, length = nbh)
  matCV <- cbind(vecth, rep(0,nbh))
  for (h in 1:nbh){
    ypred <- rep(0, n)
    for (i in 1:n){
      ypred[i]<- stats::ksmooth(x[-i], y[-i], kernel = "normal",
                                band=vecth[h], x.points=x[i])$y
    }
    matCV[h,2]<-sum((y-ypred)^2)
  }
  hopt <- matCV[which(matCV[,2] == min(matCV[,2], na.rm=TRUE)),1]
  if (graph.CV==TRUE){
    graphics::plot(matCV, xlab="bandwidth", ylab="CV MSE", pch=" ", type="l")
    graphics::abline(v = hopt, col = 2)
  }
  list(matCV = matCV, hopt = hopt)
}
