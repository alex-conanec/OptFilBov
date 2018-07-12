bovCDC <- function(X, objectifs, constraint, scale = FALSE){

  k <- length(objectifs)
  res <- as.data.frame(matrix(NA, ncol = (k + 1), nrow = nrow(X)))
  colnames(res) <- c(names(objectifs), 'constraint_respected')

  for (i in 1:nrow(X)){

    res[i, k + 1] <- all(X[i, constraint$index_plus] < constraint$seuil_plus &
                           X[i, constraint$index_minus] > constraint$seuil_minus)
  }


  if (scale) X <- scale(X)

  for (p in 1:k){

    for (i in 1:nrow(X)){

      res[i,p] <- X[i, objectifs[[p]]$index] %*% objectifs[[p]]$w

    }
  }


  res
}


weight_aggregation <- function(X, w, n = 5){

  #standardise X
  Xstd <- scale(X)

  #create a utilily index U with weight
  U <- Xstd %*% w

  #filter the n best solutions

  list(value = X[which(U %in% sort(U, decreasing = T)[1:n]),],
       index = which(U %in% sort(U, decreasing = T)[1:n]))

}

MinMax <- function(X, target, n){

  res <- rowSums(t(t(X) - target)^2)

  list(value = X[which(res %in% sort(res, decreasing = F)[1:n]),],
       index = which(res %in% sort(res, decreasing = F)[1:n]))


}


#pareto finding
pareto_finding <- function(X, Y, method = 'target', w, target, n = 5, plot = TRUE){


  if (!(method %in% c("target", "weight_aggregation")))
    stop("The function allows only method: \"target\", \"weight_aggregation\"",
         call. = FALSE)

  if (method == 'target'){
    if (missing(target))
      stop("With the method \"target\" you have to pass a target vector",
           call. = FALSE)

    decision <- MinMax(X = Y, target = target, n = n)
  }

  if (method == 'weight_aggregation'){
    if (missing(w))
      stop("With the method \"weight_aggregation\" you have to pass a w vector",
           call. = FALSE)

    decision <- weight_aggregation(X = Y, w = w, n = n)
  }

  if (plot == TRUE){

    plot(Y, col = c('red', 'blue')[as.factor(1:nrow(Y) %in% decision$index)])

    if (method == 'target'){

      points(x = target[1], y = target[2], pch = 4)
      text(x = target[1]+0.2, y = target[2]+0.2, labels = 'T')
    }

    if (method == 'weight_aggregation'){

      slope <- -w[1]/w[2]
      lower_point <- Y[decision$index[which(decision$value[,2] == min(decision$value[,2]))],]
      intercept = lower_point[2] - lower_point[1] * slope
      abline(coef = c(intercept, slope))

    }

    # X <- as.data.frame(X)
    # X$X <- 1:nrow(X)
    # Xtr <- tidyr::gather(data = X[decision$index, ], cluster, value, -X)
    # Xtr$index <- sort(rep(1:(ncol(X)-1), n))
    # Xtr$X <- as.factor(Xtr$X)
    # Xlabels <- unique(Xtr[,2])
    # Xtr <- Xtr[,-2]
    # nfactor <- length(unique(Xtr$X))
    #
    #
    # plot(range(Xtr$index), range(Xtr$value), type = "n", xaxt='n', xlab = '', ylab = '')
    #
    # axis(side = 1, at = unique(Xtr$index), labels = F)
    # text(x = unique(Xtr$index), y = min(Xtr$value) - 3,
    #      labels = Xlabels, srt = 90, xpd = T)
    #
    # colors <- rainbow(nfactor)
    #
    # for (i in 1:nfactor) {
    #   factor <- Xtr[which(as.numeric(Xtr$X) == i),]
    #   lines(factor$index, factor$value, type = "b", lwd = 1.5,
    #         col = colors[i], pch = 19)
    # }


    list(X = X[as.numeric(rownames(Y[decision$index,])),],
         Y = decision$value)
  }

}

