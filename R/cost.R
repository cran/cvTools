# ----------------------
# Author: Andreas Alfons
#         K.U.Leuven
# ----------------------

#' Prediction loss
#' 
#' Compute the prediction loss of a model.  
#' 
#' \code{mspe} and \code{rmspe} compute the mean squared prediction error and 
#' the root mean squared prediction error, respectively.  In addition, 
#' \code{mape} returns the mean absolute prediction error, which is somewhat 
#' more robust.
#' 
#' Robust prediction loss based on trimming is implemented in \code{tmspe} and 
#' \code{rtmspe}.  To be more precise, \code{tmspe} computes the trimmed mean 
#' squared prediction error and \code{rtmspe} computes the root trimmed mean 
#' squared prediction error.  A proportion of the largest squared differences 
#' of the observed and fitted values are thereby trimmed.
#' 
#' @rdname cost
#' @name cost
#' 
#' @param y  a numeric vector or matrix giving the observed values.
#' @param yHat  a numeric vector or matrix of the same dimensions as \code{y} 
#' giving the fitted values.
#' @param trim  a numeric value giving the trimming proportion (the default is 
#' 0.25).
#' 
#' @return A numeric value giving the prediction loss.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{\link{cvFit}}, \code{\link{cvTuning}}
#' 
#' @examples 
#' # fit an MM-regression model
#' library("robustbase")
#' data("coleman")
#' fit <- lmrob(Y~., data=coleman)
#' 
#' # compute the prediction loss
#' mspe(coleman$Y, predict(fit))
#' rmspe(coleman$Y, predict(fit))
#' mape(coleman$Y, predict(fit))
#' tmspe(coleman$Y, predict(fit), trim=0.1)
#' rtmspe(coleman$Y, predict(fit), trim=0.1)
#' 
#' @keywords utilities

NULL

## mean squared prediction error
#' @rdname cost
#' @export
mspe <- function(y, yHat) {
    residuals2 <- (y - yHat)^2  # squared residuals
    if(!is.null(dim(y))) {
        residuals2 <- rowSums(residuals2)  # squared norm in multivariate case
    }
    mean(residuals2)
}

## root mean squared prediction error
#' @rdname cost
#' @export
rmspe <- function(y, yHat) sqrt(mspe(y, yHat))

## mean absolute prediction error
#' @rdname cost
#' @export
mape <- function(y, yHat) {
    absResiduals <- abs(y - yHat)  # absolue residuals
    if(!is.null(dim(y))) {
        absResiduals <- rowSums(absResiduals)  # norm in multivariate case
    }
    mean(absResiduals)
}

## trimmed mean squared prediction error
#' @rdname cost
#' @export
tmspe <- function(y, yHat, trim=0.25) {
    n <- nobs(y)
    h <- n - floor(n*trim)
    residuals2 <- (y - yHat)^2  # squared residuals
    if(!is.null(dim(y))) {
        residuals2 <- rowSums(residuals2)  # squared norm in multivariate case
    }
    residuals2 <- sort(residuals2)[1:h]  # select h smallest values
    mean(residuals2)
}

## root trimmed mean squared prediction error
#' @rdname cost
#' @export
rtmspe <- function(y, yHat, trim=0.25) sqrt(tmspe(y, yHat, trim=trim))
