###########################position legend####################################
coord_legend <- function(X, Y, legend_cex = 0.6, prop_legend = c(0.20,0.36)){

  legend_prop_cex <- c(prop_legend[1] * (legend_cex/0.6),
                       prop_legend[2] * (legend_cex/0.6))

  xrange <- range(X)[2] - range(X)[1]
  yrange <- range(X)[2] - range(X)[1]

  xmin <- min(X) + legend_prop_cex[1] * xrange
  xmax <- max(X) - legend_prop_cex[1] * xrange
  ymin <- min(Y) + legend_prop_cex[2] * yrange
  ymax <- max(Y) - legend_prop_cex[2] * yrange

  left_bottom <- X < xmin & Y < ymin
  left_top <- X < xmin & Y > ymax
  right_top <- X > xmax & Y > ymax
  right_bottom <- X > xmax & Y < ymin

  optimum  <- min(length(which(left_bottom)),
                  length(which(left_top)),
                  length(which(right_bottom)),
                  length(which(right_top)))

  if (length(which(left_bottom)) == optimum) return(c(min(X), ymin))
  if (length(which(left_top)) == optimum) return(c(min(X), max(Y)))
  if (length(which(right_bottom)) == optimum) return(c(xmax, ymin))
  if (length(which(right_top)) == optimum) return(c(xmax, max(Y)))
}


#display correlation
correlation_alex <- function(data){

  panel_hist <- function(x){

    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan")
  }

  panel_cor <- function(x, y, digits = 2, prefix = '', cex.cor){

    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0,1,0,1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = '')
    if (missing(cex.cor)) cex.cor <- 2.5/strwidth(txt)
    text(0.5,0.5, txt, cex = cex.cor*r)
  }

  panel_lm <- function(x, y){
    points(x, y)
    abline(lm(x~y))
  }

  pairs(data, diag.panel = panel_hist, cex.labels = 1.3, font.labels = 2,
        upper.panel = panel_cor, lower.panel = panel_lm)

}
