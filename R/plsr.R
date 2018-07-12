#file for plsr model
pls_reg <- function(X, Y, Ylabel = NULL){


  model <- pls::plsr(Y~., data = data.frame(X),
                     validation = "CV", scale = TRUE)
  rmsep <- sqrt(model$validation$PRESS/nrow(X))
  ncomp <- find_cpt(rmsep)

  model$ncomp <- ncomp
  model$Xlabels <- colnames(X)
  model$Ylabel <- Ylabel
  model$y_train <- Y
  model$x_train <- X
  class(model) <- 'pls_reg'
  model
}

#predict pls_reg
predict.pls_reg <- function(model, newdata, predict.all = FALSE, R = 100, interval = FALSE, interval_method = 'sd', ...){

  model_cl_orig <- model

  class(model_cl_orig) <- 'mvr' #class plsr
  res <- predict(model_cl_orig, as.data.frame(newdata), ncomp = model$ncomp, ...)
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
      model$all_models <- underModels.pls_reg(model, B = R)
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


#underModel.reg_lm ----
underModels.pls_reg <- function(model, B = 100, ...){

  all_models <- list()
  for (b in 1:B){
    train <- base::sample(1:length(model$y_train), replace = TRUE)
    Y_train <- model$y_train[train]
    X_train <- model$x_train[train,]

    model_b <- pls_reg(X = X_train, Y = Y_train, Ylabel = model$Ylabel)
    all_models[[b]] <- model_b
  }

  all_models
}


#summary
summary.pls_reg <- function(object, ...){

  class(object) <- 'mvr'
  summary(object)

}

#find_cpt
find_cpt <- function(x) {
  x <- as.vector(x)
  if (length(x) >= 4) {
    cpt <- changepoint::cpt.meanvar(x,
                                    class=TRUE)@cpts[1]
  } else {
    if (length(x)==1)
      cpt=1 else
        cpt <- changepoint::cpt.mean(x,
                                     class=TRUE)@cpts[1]
  }
  cpt
}
