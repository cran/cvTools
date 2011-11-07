\name{cvExamples}
\alias{cvExamples}
\alias{cvLm}
\alias{cvLmrob}
\alias{cvLts}
\title{Cross-validation for linear models}
\usage{
 cvLm(object, cost = rmspe, K = 5, R = 1, folds = NULL,
     seed = NULL, ...)

 cvLmrob(object, cost = rtmspe, K = 5, R = 1, folds = NULL,
     seed = NULL, ...)

 cvLts(object, cost = rtmspe, K = 5, R = 1, folds = NULL,
     fit = c("reweighted", "raw", "both"), seed = NULL,
     ...)
}
\arguments{
  \item{object}{for \code{cvLm}, an object of class
  \code{"lm"} computed with \code{\link[stats]{lm}}.  For
  \code{cvLmrob}, an object of class \code{"lmrob"}
  computed with \code{\link[robustbase]{lmrob}}.  For
  \code{cvLts}, an object of class \code{"lts"} computed
  with \code{\link[robustbase]{ltsReg}}.}

  \item{cost}{a cost function measuring prediction loss.
  It should expect the observed values of the response to
  be passed as the first argument and the predicted values
  as the second argument, and must return a non-negative
  scalar value.  The default is to use the root mean
  squared prediction error for \code{cvLm} and the root
  trimmed mean squared prediction error for \code{cvLmrob}
  and \code{cvLts} (see \code{\link{cost}}).}

  \item{K}{an integer giving the number of groups into
  which the data should be split (the default is five).
  Keep in mind that this should be chosen such that all
  groups are of approximately equal size.  Setting \code{K}
  equal to \code{n} yields leave-one-out cross-validation.}

  \item{R}{an integer giving the number of replications for
  repeated \eqn{K}-fold cross-validation.  This is ignored
  for for leave-one-out cross-validation.}

  \item{folds}{an object of class \code{"cvFolds"} giving
  the folds of the data for cross-validation (as returned
  by \code{\link{cvFolds}}).  If supplied, this is
  preferred over \code{K} and \code{R}.}

  \item{fit}{a character string specifying for which fit to
  estimate the prediction error.  Possible values are
  \code{"reweighted"} (the default) for the prediction
  error of the reweighted fit, \code{"raw"} for the
  prediction error of the raw fit, or \code{"both"} for the
  prediction error of both fits.}

  \item{seed}{optional initial seed for the random number
  generator (see \code{\link{.Random.seed}}).}

  \item{\dots}{additional arguments to be passed to the
  prediction loss function \code{cost}.}
}
\value{
  An object of class \code{"cv"} with the following
  components:

  \item{n}{an integer giving the number of observations.}

  \item{K}{an integer giving the number of folds.}

  \item{R}{an integer giving the number of replications.}

  \item{cv}{a numeric vector containing the estimated
  prediction errors.  For \code{cvLm} and \code{cvLmrob},
  this is a single numeric value.  For \code{cvLts}, this
  contains one value for each of the requested fits.  In
  the case of repeated cross-validation, those are average
  values over all replications.}

  \item{reps}{a numeric matrix containing the estimated
  prediction errors from all replications.  For \code{cvLm}
  and \code{cvLmrob}, this is a matrix with one column.
  For \code{cvLts}, this contains one column for each of
  the requested fits.  However, this is only returned for
  repeated cross-validation.}

  \item{seed}{the seed of the random number generator
  before cross-validation was performed.}

  \item{call}{the matched function call.}
}
\description{
  Estimate the prediction error of a linear model via
  (repeated) \eqn{K}-fold cross-validation.
  Cross-validation functions are available for least
  squares fits computed with \code{\link[stats]{lm}} as
  well as for the following robust alternatives: MM-type
  models computed with \code{\link[robustbase]{lmrob}} and
  least trimmed squares fits computed with
  \code{\link[robustbase]{ltsReg}}.
}
\details{
  (Repeated) \eqn{K}-fold cross-validation is performed in
  the following way.  The data are first split into \eqn{K}
  previously obtained blocks of approximately equal size.
  Each of the \eqn{K} data blocks is left out once to fit
  the model, and predictions are computed for the
  observations in the left-out block with the
  \code{\link[stats]{predict}} method of the fitted model.
  Thus a prediction is obtained for each observation.

  The response variable and the obtained predictions for
  all observations are then passed to the prediction loss
  function \code{cost} to estimate the prediction error.
  For repeated cross-validation, this process is replicated
  and the estimated prediction errors from all replications
  as well as their average are included in the returned
  object.
}
\note{
  Those are simple wrapper functions that extract the data
  from the fitted model and call \code{\link{cvFit}} to
  perform cross-validation.
}
\examples{
library("robustbase")
data("coleman")
set.seed(1234)  # set seed for reproducibility

# set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

# perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

# perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman)
cvLmrob(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)

# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvLts(fitLts, cost = rtmspe, folds = folds, trim = 0.1)
cvLts(fitLts, cost = rtmspe, folds = folds, 
    fit = "both", trim = 0.1)
}
\author{
  Andreas Alfons
}
\seealso{
  \code{\link{cvFit}}, \code{\link{cvFolds}},
  \code{\link{cost}}, \code{\link[stats]{lm}},
  \code{\link[robustbase]{lmrob}},
  \code{\link[robustbase]{ltsReg}}
}
\keyword{utilities}

