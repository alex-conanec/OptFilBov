% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge.R
\name{underModels.ridge}
\alias{underModels.ridge}
\title{Generate undermodels to 'ridge' class object}
\usage{
underModels.ridge(model, B = 100, ...)
}
\arguments{
\item{model}{'ridge' class object}

\item{B}{numeric value which is the number of undermodels generate}

\item{...}{further arguments passed to or from other methods.}
}
\value{
a list of 'ridge' models
}
\description{
Allow to generate undermodels by bootstraping the dataset used for training.
}
\examples{
library(mfe)
data(indicateurs)
X <- indicateurs[, -c(1,2,3)]
Y <- indicateurs[,1]
model <- ridge(X = X, Y = Y, Ylabel = colnames(indicateurs)[1])
model$undermodels <- undermodels.ridge(model, B = 10)

}
\seealso{
\code{\link{ridge}}
}
