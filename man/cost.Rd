\name{cost}
\alias{cost}
\alias{mape}
\alias{mspe}
\alias{rmspe}
\alias{rtmspe}
\alias{tmspe}
\title{Prediction loss}
\usage{
 mspe(y, yHat)

 rmspe(y, yHat)

 mape(y, yHat)

 tmspe(y, yHat, trim = 0.25)

 rtmspe(y, yHat, trim = 0.25)
}
\arguments{
  \item{y}{a numeric vector or matrix giving the observed
  values.}

  \item{yHat}{a numeric vector or matrix of the same
  dimensions as \code{y} giving the fitted values.}

  \item{trim}{a numeric value giving the trimming
  proportion (the default is 0.25).}
}
\value{
  A numeric value giving the prediction loss.
}
\description{
  Compute the prediction loss of a model.
}
\details{
  \code{mspe} and \code{rmspe} compute the mean squared
  prediction error and the root mean squared prediction
  error, respectively.  In addition, \code{mape} returns
  the mean absolute prediction error, which is somewhat
  more robust.

  Robust prediction loss based on trimming is implemented
  in \code{tmspe} and \code{rtmspe}.  To be more precise,
  \code{tmspe} computes the trimmed mean squared prediction
  error and \code{rtmspe} computes the root trimmed mean
  squared prediction error.  A proportion of the largest
  squared differences of the observed and fitted values are
  thereby trimmed.
}
\examples{
# fit an MM-regression model
library("robustbase")
data("coleman")
fit <- lmrob(Y~., data=coleman)

# compute the prediction loss
mspe(coleman$Y, predict(fit))
rmspe(coleman$Y, predict(fit))
mape(coleman$Y, predict(fit))
tmspe(coleman$Y, predict(fit), trim=0.1)
rtmspe(coleman$Y, predict(fit), trim=0.1)
}
\author{
  Andreas Alfons
}
\seealso{
  \code{\link{cvFit}}, \code{\link{cvTuning}}
}
\keyword{utilities}

