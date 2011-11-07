# ----------------------
# Author: Andreas Alfons
#         K.U.Leuven
# ----------------------

#' Reshape cross-validation results
#' 
#' Reshape cross-validation results into an object of class \code{"cvSelect"}
#' with only one column of results.
#' 
#' @aliases cvReshape.cv cvReshape.cvSelect
#' 
#' @param x  an object inheriting from class \code{"cv"} or \code{"cvSelect"} 
#' that contains cross-validation results.
#' 
#' @returnClass cvSelect
#' @returnItem n  an integer giving the number of observations.
#' @returnItem K  an integer giving the number of folds used in 
#' cross-validation.
#' @returnItem R  an integer giving the number of replications used in 
#' cross-validation.
#' @returnItem best  an integer giving the index of the model with the best 
#' prediction performance.
#' @returnItem cv  a data frame containing the estimated prediction errors for 
#' the models.  For repeated cross-validation, those are average values over 
#' all replications.
#' @returnItem reps  a data frame containing the estimated prediction errors 
#' for the models from all replications.  This is only returned if repeated 
#' cross-validation was performed for at least one of the models.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{\link{cvFit}}, \code{\link{cvSelect}}, \code{\link{cvTuning}}
#' 
#' @example inst/doc/examples/example-cvReshape.R
#' 
#' @keywords utilities
#' 
#' @export

cvReshape <- function(x) UseMethod("cvReshape")

#' @S3method cvReshape cv
#' @S3method cvReshape cvSelect
cvReshape.cv <- cvReshape.cvSelect <- function(x) {
    # initializations
    if(ncv(x) == 0 || isTRUE(nfits(x) == 0)) stop("empty object")
    cvNames <- cvNames(x)
    # create list of objects with one column
    cvName <- defaultCvNames(1)
    objects <- lapply(cvNames, 
        function(s) {
            xs <- subset(x, select=s)
            cvNames(xs) <- cvName
            xs
        })
    # substitute "CV" in default names by "Fit"
    if(identical(cvNames, defaultCvNames(length(cvNames)))) {
        fitName <- defaultFitNames(1)
        cvNames <- gsub(cvName, fitName, cvNames, fixed=TRUE)
    }
    # call cvSelect() to combine the model fits
    names(objects) <- cvNames
    out <- do.call(cvSelect, objects)
    if(length(out$K) > 1) out$K <- out$K[1]
    if(length(out$R) > 1) out$R <- out$R[1]
    out
}
