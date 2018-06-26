#predict a cow ----
predict_cow <- function(cow_simulated, models, list_index, list_value, B=5, R=10){

  cow_simulated_init <- cow_simulated
  k <- 1
  for (i in list_index){
    cow_simulated_init[i] <- list_value[k]
    k <- k+1
  }

  cow_simulated_init <- as.data.frame(cow_simulated_init)
  res <- matrix(NA, ncol = length(cow_simulated), nrow = B)
  for (l in 1:B){

    set.seed(B)
    order_prediction <- sample((1:length(cow_simulated))[-list_index], replace = F)
    cow_simulated <- cow_simulated_init

    for (j in 1:R){
      for (i in order_prediction){

        cow_simulated[i] <-  if (is.na(predict(models[[i]], cow_simulated))) {
          cow_simulated[i]

        }else predict(model = models[[i]], newdata = cow_simulated)

      }
    }
    res[l,] <- as.matrix(cow_simulated)
  }

  res <- as.matrix(apply(res, MARGIN = 2, FUN = mean))
  rownames(res) <- colnames(cow_simulated)

  res <- t(res)
  class(res) <- 'cow'
  res
}



#plot.cow----
plot.cow <- function(cow, models, choice = 'barplot', label_angle, fixed_value, radial.lim, R = 100, ...){

  if (choice == 'barplot'){

    #generate the intervals
    intervals <- matrix(0, ncol = 2, nrow = length(models))
    for (i in 1:length(models)){

      attr(cow, "class") <- NULL

      interval_i <- pred_interval(model = models[[i]],
                                  newdata = as.data.frame(cow),
                                  method = 'qt',
                                  R = R)

      intervals[i,1] <- interval_i[1]
      intervals[i,2] <- interval_i[2]
    }


    b <- barplot(cow,
                 xaxt = 'n',
                 space = c(0,0.5),
                 col = 'blue',
                 beside = TRUE, ...)

    #add the axis labels
    text(x = b-0.15,
         par("usr")[3],
         labels = colnames(cow),
         srt = label_angle,
         pos = 1,
         cex = 1,
         xpd = T)

    #add the arrows for the prediction interval
    if (length(fixed_value) > 0) b[,fixed_value] <- NA

    if (! is.null(intervals)){
      arrows(x0 = b,
             x1 = b,
             y0 = intervals[,1],
             y1 = intervals[,2],
             angle = 90,
             code = 3,
             col = 'red',
             length = 0.1)
    }

  }

  if (choice == 'radar_diag'){

    plotrix::radial.plot(lengths = cow,
                         labels = colnames(cow),
                         rp.type = "p",
                         lwd = 3,
                         rad.col = 'blue',
                         radial.lim = radial.lim,
                         start = 1.5708,
                         clockwise = TRUE,
                         line.col = "red")

  }
}


#pred interval
pred_interval <- function(model, newdata, method = 'sd', R = 100){

  if (class(model) == "reg_lm"){
    predict(model, newdata, interval = "prediction")[-1]
  }else{
    pred_all <- predict(model, newdata, predict.all = TRUE, R = R)
    res <- apply(pred_all$individual, 2, function(x) {
      c(mean(x, na.rm = TRUE) + c(-1.96,1.96) * sd(x, na.rm = TRUE),
        quantile(x, c(0.025,0.975), na.rm = TRUE) )
    })
    if (method == 'sd') return(t(res)[,1:2]) else return(t(res)[,3:4])
  }

}

