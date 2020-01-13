## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  install.packages("relgam")

## ------------------------------------------------------------------------
library(relgam)

## ------------------------------------------------------------------------
set.seed(1)
n <- 100; p <- 12
x = matrix(rnorm((n) * p), ncol = p)
f4 = 2 * x[,4]^2 + 4 * x[,4] - 2
f5 = -2 * x[, 5]^2 + 2
f6 = 0.5 * x[, 6]^3
mu = rowSums(x[, 1:3]) + f4 + f5 + f6
y = mu + sqrt(var(mu) / 4) * rnorm(n)

## ------------------------------------------------------------------------
fit <- rgam(x, y, verbose = FALSE)

## ------------------------------------------------------------------------
fit <- rgam(x, y, init_nz = c(), verbose = FALSE)

## ------------------------------------------------------------------------
fit <- rgam(x, y, gamma = 0.6, df = 8, verbose = FALSE)

## ------------------------------------------------------------------------
# no. of features/linear components/non-linear components for 20th lambda value
fit$nzero_feat[20]
fit$nzero_lin[20]
fit$nzero_nonlin[20]

## ------------------------------------------------------------------------
# features included in the model for 20th lambda value
fit$feat[[20]]
# features which have a linear component in the model for 20th lambda value
fit$linfeat[[20]]
# features which have a non-linear component in the model for 20th lambda value
fit$nonlinfeat[[20]]

## ------------------------------------------------------------------------
# get predictions for 20th model for first 5 observations
predict(fit, x[1:5, ])[, 20]

## ------------------------------------------------------------------------
f5 <- getf(fit, x, j = 5, index = 20)
as.vector(f5)

## ------------------------------------------------------------------------
plot(x[, 5], f5, xlab = "x5", ylab = "f(x5)")

## ------------------------------------------------------------------------
fit <- rgam(x, y, verbose = FALSE)

## ----fig.width = 7-------------------------------------------------------
par(mfrow = c(1, 4))
par(mar = c(4, 2, 2, 2))
plot(fit, x)

## ----fig.width = 7-------------------------------------------------------
# show fitted functions for x2, x5 and x8 at the model for the 15th lambda value
par(mfrow = c(1, 3))
plot(fit, x, index = 15, which = c(2, 5, 8))

## ------------------------------------------------------------------------
summary(fit)

## ------------------------------------------------------------------------
# coefficient profiles for just features 4 to 6
summary(fit, which = 1:6, label = TRUE)

## ------------------------------------------------------------------------
cvfit <- cv.rgam(x, y, init_nz = 1:ncol(x), gamma = 0.6, verbose = FALSE)

## ------------------------------------------------------------------------
cvfit <- cv.rgam(x, y, init_nz = 1:ncol(x), gamma = 0.6, nfolds = 5, verbose = FALSE)

## ------------------------------------------------------------------------
set.seed(3)
foldid <- sample(rep(seq(10), length = n))
cvfit <- cv.rgam(x, y, init_nz = 1:ncol(x), gamma = 0.6, foldid = foldid, verbose = FALSE)

## ----fig.width=5, fig.height=4-------------------------------------------
plot(cvfit)

## ------------------------------------------------------------------------
cvfit$lambda.min
cvfit$lambda.1se

## ------------------------------------------------------------------------
predict(cvfit, x[1:5, ])   # s = lambda.1se
predict(cvfit, x[1:5, ], s = "lambda.min")

## ------------------------------------------------------------------------
# fit binary model
bin_y <- ifelse(y > 0, 1, 0)
binfit <- rgam(x, bin_y, family = "binomial", init_nz = c(), gamma = 0.9, 
              verbose = FALSE)

# linear predictors for first 5 observations at 10th model
predict(binfit, x[1:5, ])[, 10]

# predicted probabilities for first 5 observations at 10th model
predict(binfit, x[1:5, ], type = "response")[, 10]

## ------------------------------------------------------------------------
# generate data
set.seed(5)
offset <- rnorm(n)
poi_y <- rpois(n, abs(mu) * exp(offset))
poifit <- rgam(x, poi_y, family = "poisson", offset = offset, verbose = FALSE)

## ------------------------------------------------------------------------
# rate predictions at 20th lambda value
predict(poifit, x[1:5, ], newoffset = offset, type = "response")[,20]

