#qlt_model
qlt_model <- function(model, B = 100){


  if (!(class(model) %in% c("reg_lm", "sir", "rf", "ridge", "pls_reg")))
    stop("The function allows only model: \"reg_lm\", \"sir\", \"rf\", \"pls_reg\", \"ridge\"",
         call. = FALSE)

  if (is.null(model$Ylabel))
    stop("\"Ylabel\" is needed",
         call. = FALSE)


  corrected_criterias <- matrix(NA, nrow = B, ncol = 3)
  colnames(corrected_criterias) <- c('R2', 'RMSE', 'R2adj')

  for (b in 1:B){

    i <- sample(nrow(model$x_train), replace = T)
    X_train <- model$x_train[i, ]
    X_test <- model$x_train
    Y_train <- model$y_train[i]
    Y_test <- model$y_train
    Ylabel <- model$Ylabel

    # fit model_b
    model_b <- switch(class(model),
                      reg_lm = reg_lm(Y = Y_train, X = X_train, Ylabel = Ylabel),
                      rf = rf(Y = Y_train, X = X_train, Ylabel = Ylabel),
                      sir = sir(Y = Y_train, X = X_train, Ylabel = Ylabel),
                      plsr = pls_reg(Y = Y_train, X = X_train, Ylabel = Ylabel),
                      ridge = ridge(Y = Y_train, X = X_train, Ylabel = Ylabel))


    # predict the values on the original, unresampled data
    Ypred_train <- predict(model_b, X_train)
    Ypred_orig <- predict(model_b, X_test)


    # criteria_b,boot - criteria_b,orig
    #R2
    corrected_criterias[b,1] <- (R2(y = Y_train,
                                    y_pred = Ypred_train)$R2
                                 -
                                 R2(y = Y_test,
                                    y_pred = Ypred_orig)$R2)

    #RMSE
    corrected_criterias[b,2] <- (rmse(y = Y_train,
                                      y_pred = Ypred_train)
                                 -
                                 rmse(y = Y_test,
                                      y_pred = Ypred_orig))

    #R2adj
    corrected_criterias[b,3] <- (R2(y = Y_train,
                                    y_pred = Ypred_train,
                                    k = length(model_b$Xlabels)
                                    )$R2adj
                                 -
                                 R2(y = Y_test,
                                    y_pred = Ypred_orig,
                                    k = length(model_b$Xlabels)
                                    )$R2adj)

  }


  #calculate the estimation real of the criterias
  O <- apply(corrected_criterias, MARGIN = 2, FUN = mean)
  R2app <- R2(y = Y_test,
              y_pred = predict(model, X_test))$R2


  RMSEapp <- rmse(y = Y_test,
             y_pred = predict(model, X_test))


  R2adjapp <- R2(y = Y_test,
                 y_pred = predict(model, X_test),
                 k = length(model$Xlabels))$R2adj


  R2_reel <- R2app - O[1]
  RMSE_reel <- RMSEapp - O[2]
  R2adj_reel <- R2adjapp - O[3]

  res <- list(qlt = list(R2 = R2app,
                    R2_reel = R2_reel,
                    RMSE = RMSEapp,
                    RMSE_reel = RMSE_reel,
                    R2adjapp = R2adjapp,
                    R2adj_reel = R2adj_reel),
              model = model)
  class(res) <- 'qlt_model'
  res
}


#R2
R2 <- function(y, y_pred, k = 1){

  SCEy <- sum((y_pred-mean(y))^2, na.rm = TRUE)
  SCEt <- sum((y-mean(y))^2, na.rm = TRUE)
  R2 <- SCEy/SCEt
  n <- length(y)
  R2adj <- 1-((1-R2)*(n-1))/(n-k-1)

  list(R2=R2, R2adj=R2adj)
}


#RMSE
rmse=function(y, y_pred){

  res <- 0
  for (i in 1:length(y)){
    res <- sum(res, (y[i] - y_pred[i])^2, na.rm = TRUE)
  }

  as.numeric(sqrt(res)/2)
}


#plot.qtl_model
plot.qlt_model <- function(qlt_model, ...){

  model <- qlt_model$model
  length_char <- length(model$Xlabels)

  regressors_label <- ''
  i=1
  while (i < length_char){

    regressors_label <- paste(regressors_label,
                              paste (model$Xlabels[i: (i+ 10)], collapse = ' + '),
                              '\n')

    i <- i + 10
  }

  regressors_label <- unlist(strsplit(regressors_label, split='NA'))[1]
  regressors_label <- paste(unlist(strsplit(regressors_label, split='+ '))[-c(1,(2*length_char +1))], collapse=' ')

  x <- model$y_train
  y <- predict(model, model$x_train)
  plot(x = x,
       y = y,
       col = c('green', 'red')[as.factor(abs(x - y) > sd(x))])

  abline(0,1, col = 'red')

  title(regressors_label, ...)


  legend(coord_legend(X = x, Y = y, prop_legend = c(0.22,0.2))[1],
         coord_legend(X = x, Y = y, prop_legend = c(0.22,0.2))[2],
         c('< sd','> sd'),
         pch = 1,
         col = c('green', 'red'))

  text(x = range(x)[1] + 0.8*(range(x)[2] - range(x)[1]),
       y = range(y)[1] + 0.1*(range(y)[2] - range(y)[1]),
       labels = paste('R²=', round(qlt_model$qlt$R2, 2)))

}
