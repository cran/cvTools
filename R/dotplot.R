# ----------------------
# Author: Andreas Alfons
#         K.U.Leuven
# ----------------------

#' Dot plots of cross-validation results
#' 
#' Produce dot plots of (average) results from (repeated) \eqn{K}-fold 
#' cross-validation.
#' 
#' For objects with multiple columns of repeated cross-validation results, 
#' conditional dot plots are produced.
#' 
#' @method dotplot cvSelect
#' 
#' @param x  an object inheriting from class \code{"cvSelect"} that contains 
#' cross-validation results.
#' @param data  currently ignored.
#' @param subset  a character, integer or logical vector indicating the subset 
#' of models for which to plot the cross-validation results.
#' @param select  a character, integer or logical vector indicating the columns 
#' of cross-validation results to be plotted.
#' @param \dots  additional arguments to be passed to the \code{"formula"} 
#' method of \code{\link[lattice:xyplot]{dotplot}}.
#' 
#' @return An object of class \code{"trellis"} is returned invisibly.  The 
#' \code{\link[lattice:update.trellis]{update}} method can be used to update 
#' components of the object and the \code{\link[lattice:print.trellis]{print}} 
#' method (usually called by default) will plot it on an appropriate plotting 
#' device.
#' 
#' @author Andreas Alfons
#' 
#' @seealso \code{\link{cvFit}}, \code{\link{cvSelect}}, 
#' \code{\link{cvTuning}}, \code{\link[=plot.cv]{plot}}, 
#' \code{\link[=xyplot.cvSelect]{xyplot}}, \code{\link[=bwplot.cv]{bwplot}}, 
#' \code{\link[=densityplot.cv]{densityplot}}
#' 
#' @example inst/doc/examples/example-dotplot.R
#' 
#' @keywords hplot
#' 
#' @import lattice
#' @export

dotplot.cvSelect <- function(x, data, subset = NULL, select = NULL, ...) {
    # construct data frame in lattice format and call internal function
    CV <- getLatticeData(x, subset, select, reps=FALSE, numericAsFactor=TRUE)
    localDotplot(CV, ...)
}


# internal function for dot plots
localDotplot <- function(CV, horizontal = TRUE, xlab = NULL, ylab = NULL, ...,
        # the following arguments are defined so that they aren't supplied twice
        x, formula, data, groups) {
    # construct formula for call to xyplot()
    cvNames <- names(CV)
    haveFit <- "Fit" %in% cvNames
    if(horizontal) {
        left <- if(haveFit) "Fit" else ""
        right <- "CV"
        if(missing(xlab)) xlab <- "CV results"
    } else {
        if(!haveFit) CV$Fit <- rep.int(defaultFitNames(1), nrow(CV))
        left <- "CV"
        right <- "Fit"
        if(missing(ylab)) ylab <- "CV results"
    }
    conditional <- if("Name" %in% cvNames) "Name" else NULL
    f <- getFormula(left, right, conditional)
    # call dotplot()
    dotplot(f, data=CV, horizontal=horizontal, xlab=xlab, ylab=ylab, ...)
}
