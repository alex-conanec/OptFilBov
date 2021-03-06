% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm.R
\name{reg_lm}
\alias{reg_lm}
\title{reg_lm models}
\usage{
reg_lm(X, Y, Ylabel = NULL)
}
\arguments{
\item{X}{numerical matrix of dimension n by p regressors}

\item{Y}{numerical vector of size n containing the response to predict.}

\item{Ylabel}{a character value containing the name of the Y varaible}
}
\description{
reg_lm is a method used to fit linear models using the lm package.
}
\examples{
library(mfe)
data(indicateurs)
X <- indicateurs[, -c(1,2,3)]
Y <- indicateurs[,1]
model <- reg_lm(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
predict(model, newdata = indicateurs[1,])


}
\seealso{
\code{\link{predict.reg_lm}}
}
