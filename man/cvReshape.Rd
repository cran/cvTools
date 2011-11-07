\name{cvReshape}
\alias{cvReshape}
\alias{cvReshape.cv}
\alias{cvReshape.cvSelect}
\title{Reshape cross-validation results}
\usage{
 cvReshape(x)
}
\arguments{
  \item{x}{an object inheriting from class \code{"cv"} or
  \code{"cvSelect"} that contains cross-validation
  results.}
}
\value{
  An object of class \code{"cvSelect"} with the following
  components:

  \item{n}{an integer giving the number of observations.}

  \item{K}{an integer giving the number of folds used in
  cross-validation.}

  \item{R}{an integer giving the number of replications
  used in cross-validation.}

  \item{best}{an integer giving the index of the model with
  the best prediction performance.}

  \item{cv}{a data frame containing the estimated
  prediction errors for the models.  For repeated
  cross-validation, those are average values over all
  replications.}

  \item{reps}{a data frame containing the estimated
  prediction errors for the models from all replications.
  This is only returned if repeated cross-validation was
  performed for at least one of the models.}
}
\description{
  Reshape cross-validation results into an object of class
  \code{"cvSelect"} with only one column of results.
}
\examples{
library("robustbase")
data("coleman")

# perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, K = 5, R = 10, 
    fit = "both", trim = 0.1, seed = 1234)
# compare original and reshaped object
cvFitLts
cvReshape(cvFitLts)
}
\author{
  Andreas Alfons
}
\seealso{
  \code{\link{cvFit}}, \code{\link{cvSelect}},
  \code{\link{cvTuning}}
}
\keyword{utilities}

