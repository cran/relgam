% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.cv.rgam.R
\name{plot.cv.rgam}
\alias{plot.cv.rgam}
\title{Plot the cross-validation curve produced by "cv.rgam" object}
\usage{
\method{plot}{cv.rgam}(x, sign.lambda = 1, ...)
}
\arguments{
\item{x}{Fitted "\code{cv.rgam}" object.}

\item{sign.lambda}{Either plot against \code{log(lambda)} (default) or
\code{-log(lambda)} (if \code{sign.lambda = -1}).}

\item{...}{Other graphical parameters to plot.}
}
\description{
Plots the cross-validation curve produced by a \code{cv.rgam} object, along
with upper and lower standard deviation curves, as a function of the \code{lambda}
values used. The plot also shows the number of non-zero features picked for
each value of lambda.
}
\details{
A plot is produced and nothing is returned.
}
\examples{
set.seed(1)
n <- 100; p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- matrix(c(rep(2, 5), rep(0, 15)), ncol = 1)
y <- x \%*\% beta + rnorm(n)

cvfit <- cv.rgam(x, y)
plot(cvfit)

}
\seealso{
\code{\link{rgam}} and \code{\link{cv.rgam}}.
}
