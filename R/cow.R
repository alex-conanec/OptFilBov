#' Predict a cow
#'
#' Predict the cow performances with the whole models
#'
#' @param cow_simulated a matrix or data.frame 1 by p attributes with the colnames matching with the models
#' @param models list of all the models describing the cow attributes
#' @param list_index vector containing the position in the data.frame of the fixed value
#' @param list_value vector containing the value at the list_index position
#' @param R integer to converge toward a stable result (10 by default)
#' @param B integer for the number of bootstrap which switch the order of prediction (5 by default)
#'
#'
#' @return a predicted cow througth a object cow
#'
#' @seealso \code{\link{plot.cow}}
#'
#'
#' @examples
#' library(mfe)
#' load(models.RData) #need models fitted
#' data(indicateurs)
#' cow0 <- setNames(data.frame(matrix(0, ncol = ncol(indicateurs), nrow = 1)), colnames(indicateurs))
#' pred_cow <- predict_cow(cow_simulated = cow0, models = models, list_index = 1:3, list_value = c(0, 1, -0.5), B=5, R=10)
#' plot(pred_cow)
#'
#'
#' @export


predict_cow <- function(cow_simulated, models, list_index, list_value, R=10, B=5){

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



#' Plot method for a object of class 'cow'
#'
#' Plot the cow attribute either a barplot with prediction interval, either a radar diagram
#'
#' @param cow a object of class 'cow' which will be plot
#' @param models list of all the models describing the cow attributes
#' @param choice choice of the graph among 'barplot' (by default) and 'radar_diag'
#' @param label_angle integer between 0 and 360 which is the angle of the x labels for the barplot
#' @param fixed_value vector containing the postion of the cow attributes which do not need prediction interval
#' @param R integer for the number of predicted value by under-models for prediction interval (100 by default).
#' Warning, if the number of the under-model is lower than R, the number of under-model will be the max.
#'
#' @seealso \code{\link{predict_cow}}
#'
#'
#' @examples
#' library(mfe)
#' load(models.RData) #need models fitted
#' data(indicateurs)
#' cow0 <- setNames(data.frame(matrix(0, ncol = ncol(indicateurs), nrow = 1)), colnames(indicateurs))
#' pred_cow <- predict_cow(cow_simulated = cow0, models = models, list_index = 1:3, list_value = c(0, 1, -0.5), B=5, R=10)
#' plot(pred_cow)
#'
#' @export


plot.cow <- function(cow, models, choice = 'barplot', label_angle, fixed_value, R=100,
                     bg_par = 'white', color="green", legend_label=NULL, cex=1, x_legend=0,
                     y_legend=0, lty=1, ...){

  par(bg = bg_par)

  if (choice == 'barplot'){

    #generate the intervals
    intervals <- matrix(0, ncol = 2, nrow = length(models))
    for (i in 1:length(models)){

      attr(cow, "class") <- NULL

      interval_i <- predict(model = models[[i]],
                            newdata = as.data.frame(cow),
                            interval  = TRUE,
                            interval_method = 'qt',
                            R = R)

      intervals[i,1] <- interval_i[1]
      intervals[i,2] <- interval_i[2]
    }


    b <- barplot(cow,
                 xaxt = 'n',
                 space = c(0,0.5),
                 col = 'blue',
                 beside = TRUE,
                 ...)

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
                         start = 1.5708,
                         clockwise = TRUE,
                         mar=c(1, 2.5, 1, 4.5),
                         line.col = color,
                         cex=cex,
                         lty=lty,
                         ...)
    if (!is.null(legend_label)) legend(x=x_legend, y=y_legend,legend = legend_label, lwd = 3, col = color, cex = cex, lty=lty)


  }
}


