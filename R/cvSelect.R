# ----------------------
# Author: Andreas Alfons
#         K.U.Leuven
# ----------------------

#' Model selection based on cross-validation
#' 
#' Combine cross-validation results for various models into one object and 
#' select the model with the best prediction performance.
#' 
#' Keep in mind that objects inheriting from class \code{"cv"} or 
#' \code{"cvSelect"} may contain multiple columns of cross-validation 
#' results.  This is the case if the response is univariate but the 
#' \code{\link[stats]{predict}} method of the fitted model returns a 
#' matrix.  
#' 
#' The \code{.reshape} argument determines how to handle such objects.  If 
#' \code{.reshape} is \code{FALSE}, all objects are required to have the same 
#' number of columns and the best model for each column is selected.  A typical 
#' use case for this behavior would be if the investigated models contain 
#' cross-validation results for a raw and a reweighted fit.  It might then be 
#' of interest to researchers to compare the best model for the raw estimators 
#' with the best model for the reweighted estimators.
#' 
#' If \code{.reshape} is \code{TRUE}, objects with more than one column of 
#' results are first transformed with \code{\link{cvReshape}} to have only one 
#' column.  Then the best overall model is selected.
#' 
#' It should also be noted that the \code{.reshape} argument starts with a dot 
#' to avoid conflicts with the argument names used for the objects containing 
#' cross-validation results.
#' 
#' @param \dots  objects inheriting from class \code{"cv"} or \code{"cvSelect"} 
#' that contain cross-validation results.
#' @param .reshape  a logical indicating whether objects with more than one 
#' column of cross-validation results should be reshaped to have only one 
#' column (see \dQuote{Details}).
#' 
#' @aliases print.cvSelect
#' 
#' @returnClass cvSelect
#' @returnItem n  an integer giving the number of observations.
#' @returnItem K  an integer vector giving the number of folds used in 
#' cross-validation for the respective model.
#' @returnItem R  an integer vector giving the number of replications used in 
#' cross-validation for the respective model.
#' @returnItem best  an integer vector giving the indices of the models with 
#' the best prediction performance.
#' @returnItem cv  a data frame containing the estimated prediction errors for 
#' the models.  For models for which repeated cross-validation was performed, 
#' those are average values over all replications.
#' @returnItem reps  a data frame containing the estimated prediction errors 
#' from all replications for those models for which repeated cross-validation 
#' was performed.  This is only returned if repeated cross-validation was 
#' performed for at least one of the models.
#' 
#' @note Even though the function allows to compare cross-validation results 
#' obtained with a different number of folds or a different number of 
#' replications, such comparisons should be made with care.  Hence warnings 
#' are issued in those cases.  For maximum comparability, the same data folds 
#' should be used in cross-validation for all models to be compared.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{\link{cvFit}}, \code{\link{cvTuning}}
#' 
#' @example inst/doc/examples/example-cvSelect.R
#' 
#' @keywords utilities
#' 
#' @export

cvSelect <- function(..., .reshape = FALSE) {
    ## initializations
    objects <- list(...)
    m <- length(objects)
    if(m == 0) stop("empty list of objects")
    # check class of objects
    isCvSelect <- sapply(objects, inherits, "cvSelect")
    if(!all(sapply(objects, inherits, "cv") | isCvSelect)) {
        stop("all objects must inherit from class \"cv\" or \"cvSelect\"")
    }
    # remove empty objects
    keep <- sapply(objects, function(x) ncv(x) > 0 && !isTRUE(nfits(x) == 0))
    objects <- objects[keep]
    m <- length(objects)
    if(m == 0) stop("all objects are empty")
    isCvSelect <- isCvSelect[keep]
    # check names for the supplied objects
    fits <- names(objects)
    if(is.null(fits)) {
        fits <- defaultFitNames(m)
    } else if(any(i <- fits == "")) fits[i] <- defaultFitNames(m)[i]
    names(objects) <- fits
    # check dimensions or reshape objects with more than one column
    d <- sapply(objects, ncv)
    if(isTRUE(.reshape)) {
        .reshape <- which(d > 1)
        objects[.reshape] <- lapply(objects[.reshape], cvReshape)
        isCvSelect[.reshape] <- TRUE
    } else if(length(unique(d)) > 1) {
        stop("all objects must have the same dimension")
    }
    ## prepare objects of class "cvSelect"
    if(any(isCvSelect)) {
        # prepare names
        fits <- as.list(fits)
        fits[isCvSelect] <- mapply(function(f, x) paste(f, x$cv$Fit, sep="."), 
            fits[isCvSelect], objects[isCvSelect], SIMPLIFY=FALSE)
        fits <- unlist(fits)
        # prepare basic information
        objects[isCvSelect] <- lapply(objects[isCvSelect], 
            function(x) {
                m <- nrow(x$cv)  # number of fits in current object
                x$n <- rep(x$n, length.out=m)
                x$K <- rep(x$K, length.out=m)
                x$R <- rep(x$R, length.out=m)
                # remove column specifying fit from results
                x$cv <- x$cv[, -1, drop=FALSE]
                if(!is.null(x$reps)) x$reps <- x$reps[, -1, drop=FALSE]
                x
            })
    }
    ## combine basic information
    n <- unique(unlist(lapply(objects, function(x) x$n), use.names=FALSE))
    if(length(n) > 1) stop("different numbers of observations")
    K <- unlist(lapply(objects, function(x) x$K), use.names=FALSE)
    if(length(unique(K)) > 1) warning("different number of folds")
    R <- unlist(lapply(objects, function(x) x$R), use.names=FALSE)
    if(length(unique(R)) > 1) warning("different number of replications")
    names(K) <- names(R) <- fits
    ## combine CV results
    cv <- lapply(objects, 
        function(x) {
            cv <- x$cv                                     # extract CV results
            if(is.null(dim(cv))) t(cv) else as.matrix(cv)  # return matrix
        })
    if(m > 1) {
        # check if names are the same for all objects
        cvNames <- colnames(cv[[1]])
        otherNames <- lapply(cv[-1], colnames)
        adjustNames <- !all(sapply(otherNames, identical, cvNames))
        if(adjustNames) cvNames <- defaultCvNames(length(cvNames))
    }
    cv <- do.call("rbind", cv)
    if(m > 1 && adjustNames) colnames(cv) <- cvNames
    cv <- data.frame(Fit=factor(fits, levels=fits), cv, row.names=NULL)
    ## combine repeated CV results
    haveReps <- any(i <- sapply(objects, function(x) !is.null(x$reps)))
    if(haveReps) {
        # FIXME: safer solution that does not require the correct order of fits
        reps <- lapply(objects[i], function(x) as.matrix(x$reps))
        reps <- do.call("rbind", reps)
        if(m > 1 && adjustNames) colnames(reps) <- cvNames
        i <- which(R > 1)
        reps <- data.frame(Fit=factor(rep(fits[i], R[i]), levels=fits), 
            reps, row.names=NULL)
    }
    ## find best model
    best <- sapply(cv[, -1, drop=FALSE], which.min)
    ## construct return object
    out <- list(n=n, K=K, R=R, best=best, cv=cv)
    if(haveReps) out$reps <- reps
    class(out) <- "cvSelect"
    out
}
