% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rgam.R
\name{rgam}
\alias{rgam}
\title{Fit reluctant generalized additive model}
\usage{
rgam(x, y, lambda = NULL, lambda.min.ratio = ifelse(nrow(x) < ncol(x),
  0.01, 1e-04), standardize = TRUE, family = c("gaussian", "binomial",
  "poisson", "cox"), offset = NULL, init_nz, removeLin = TRUE,
  nfolds = 5, foldid = NULL, df = 4, gamma, tol = 0.01,
  parallel = FALSE, verbose = TRUE)
}
\arguments{
\item{x}{Input matrix, of dimension \code{nobs x nvars}; each row is
an observation vector.}

\item{y}{Response variable. Quantitative for \code{family = "gaussian"} or
\code{family = "poisson"} (non-negative counts). For \code{family="binomial"},
should be a numeric vector consisting of 0s and 1s. For \code{family="cox"},
y should be a two-column matrix with columns named 'time' and 'status'.
The latter is a binary variable, with '1' indicating death, and '0'
indicating right-censored.}

\item{lambda}{A user-supplied \code{lambda} sequence. Typical usage is to
have the program compute its own \code{lambda} sequence; supplying a value of
lambda overrides this.}

\item{lambda.min.ratio}{Smallest value for lambda as a fraction of the
largest lambda value. The default depends on the sample size nobs relative to
the number of variables nvars. If nobs > nvars, the default is 0.0001,
close to zero. If nobs < nvars, the default is 0.01.}

\item{standardize}{If \code{TRUE} (default), the columns of the input matrix
are standardized before the algorithm is run. See details section for more
information.}

\item{family}{Response type. Either \code{"gaussian"} (default) for linear
regression, \code{"binomial"} for logistic regression, \code{"poisson"} for
Poisson regression or \code{"cox"} for Cox regression.}

\item{offset}{A vector of length nobs. Useful for the "poisson" family (e.g.
log of exposure time), or for refining a model by starting at a current fit.
Default is NULL. If supplied, then values must also be supplied to the predict
function.}

\item{init_nz}{A vector specifying which features we must include when
computing the non-linear features. Default is to construct non-linear features
for all given features.}

\item{removeLin}{When constructing the non-linear features, do we remove
the linear component from them? Default is \code{TRUE}.}

\item{nfolds}{Number of folds for CV in Step 1 (default is 5). Although
\code{nfolds} can be as large as the sample size (leave-one-out CV), it is
not recommended for large datasets. Smallest value allowable is \code{nfolds = 3}.}

\item{foldid}{An optional vector of values between 1 and \code{nfolds}
identifying what fold each observation is in. If supplied, \code{nfolds} can
be missing.}

\item{df}{Degrees of freedom for the non-linear fit in Step 2. Default is 4.}

\item{gamma}{Scale factor for non-linear features (vs. original features), to
be between 0 and 1. Default is 0.8 if \code{init_nz = c()}, 0.6 otherwise.}

\item{tol}{Parameter to be passed to \code{smooth.spline}: a tolerance for
same-ness or uniqueness of the x values. Default is 0.01. See
\code{smooth.spline} documentation for more details.}

\item{parallel}{If TRUE, the \code{cv.glmnet()} call in Step 1 is
parallelized. Must register parallel before hand, such as doMC or others.
Default is FALSE.}

\item{verbose}{If \code{TRUE} (default), model-fitting is tracked with a
progress bar.}
}
\value{
An object of class \code{"rgam"}.
\item{full_glmfit}{The glmnet object resulting from Step 3: fitting a \code{glmnet}
model for the response against the linear & non-linear features.}
\item{spline_fit}{List of spline fits for residual against each response.
Needed for predicting on new data.}
\item{lin_comp_fit}{If \code{removeLin = TRUE}, a list of coefficients for
simple linear regression of non-linear feature on original feature. Needed
for predicting on new data.}
\item{init_nz}{Column indices for the features which we allow to have
non-linear relationship with the response.}
\item{step1_nz}{Indices of features which CV in Step 1 chose.}
\item{removeLin}{Did we remove the linear components when constructing the
non-linear features? Needed for predicting on new data.}
\item{mxf}{Means of the features (both linear and non-linear).}
\item{sxf}{Scale factors of the features (both linear and non-linear).}
\item{feat}{Column indices of the non-zero features for each value of
\code{lambda}.}
\item{linfeat}{Column indices of the non-zero linear components for each value of
\code{lambda}.}
\item{nonlinfeat}{Column indices of the non-zero non-linear components for each value
of \code{lambda}.}
\item{nzero_feat}{The number of non-zero features for each value of
\code{lambda}.}
\item{nzero_lin}{The number of non-zero linear components for each value of
\code{lambda}.}
\item{nzero_nonlin}{The number of non-zero non-linear components for each value
of \code{lambda}.}
\item{lambda}{The actual sequence of \code{lambda} values used.}
\item{p}{The number of features in the original data matrix.}
\item{family}{Response type.}
\item{call}{The call that produced this object.}
}
\description{
Fits a reluctant generalized additive model (RGAM) for an entire regularization
path indexed by the parameter \code{lambda}. Fits linear, logistic, Poisson
and Cox regression models. RGAM is a three-step algorithm: Step 1 fits the
lasso and computes residuals, Step 2 constructs the non-linear features, and
Step 3 fits a lasso of the response on both the linear and non-linear features.
}
\details{
If there are variables which the user definitely wants to compute non-linear
versions for in Step 2 of the algorithm, they can be specified as a vector for
the \code{init_nz} option. The algorithm will compute non-linear versions for
these features as well as the features suggested by Step 1 of the algorithm.

If \code{standardize = TRUE}, the standard deviation of the linear and
non-linear features would be 1 and gamma respectively. If
\code{standardize = FALSE}, linear features will remain on their original
scale while non-linear features would have standard deviation gamma times
the mean standard deviation of the linear features.

For \code{family="gaussian"}, \code{rgam} standardizes \code{y} to have unit
variance (using \code{1/n} rather than \code{1/(n-1)} formula).
}
\examples{
set.seed(1)
n <- 100; p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- matrix(c(rep(2, 5), rep(0, 15)), ncol = 1)
y <- x \%*\% beta + rnorm(n)

fit <- rgam(x, y)

# construct non-linear features for only those selected by Step 1
fit <- rgam(x, y, init_nz = c())

# specify scale factor gamma and degrees of freedom
fit <- rgam(x, y, gamma = 1, df = 6)

# binomial family
bin_y <- ifelse(y > 0, 1, 0)
fit2 <- rgam(x, bin_y, family = "binomial")

# Poisson family
poi_y <- rpois(n, exp(x \%*\% beta))
fit3 <- rgam(x, poi_y, family = "poisson")
# Poisson with offset
offset <- rnorm(n)
fit3 <- rgam(x, poi_y, family = "poisson", offset = offset)

}
