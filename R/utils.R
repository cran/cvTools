# ----------------------
# Author: Andreas Alfons
#         K.U.Leuven
# ----------------------

## utilities for cross-validation functions

# retrieve the number of observations
nobs.default <- function(object, ...) {
    n <- nrow(object)                   # matrix or data.frame
    if(is.null(n)) n <- length(object)  # vector
    n
}

# retrieve data subsets
dataSubset <- function(x, i, drop = FALSE) {
    if(is.null(dim(x))) {
        x[i]
    } else x[i, , drop=FALSE]
}

# replace data subsets
"dataSubset<-" <- function(x, i, value) {
    if(is.null(dim(x))) {
        x[i] <- value
    } else x[i, ] <- value
    x
}

# combine data (used for predictions from CV folds)
combineData <- function(x) {
    if(is.null(dim(x[[1]]))) {
        unlist(x)
    } else do.call("rbind", x)
}

## call a function by either
# 1) simply evaluating a supplied function for the basic arguments if there are
#    no additional arguments in list format
# 2) evaluating a supplied function with 'do.call' if there are additional 
#    arguments in list format
doCall <- function(fun, ..., args) {
    if(length(args) == 0) {
        fun(...)
    } else do.call(fun, c(list(...), args))
}

# default names
defaultCvNames <- function(p) {
    if(p == 1) {
        "CV"
    } else if(p > 0) {
        paste("CV", seq_len(p), sep="")
    } else character()
}
defaultFitNames <- function(m) {
    if(m == 1) {
        "Fit"
    } else if(m > 0) {
        paste("Fit", seq_len(m), sep="")
    } else character()
}

# add intercept column to design matrix
addIntercept <- function(x) {
    cbind("(Intercept)"=rep.int(1, nrow(x)), x)
}

## add intercept column to design matrix
addIntercept <- function(x, check = FALSE) {
    if(!check || all(is.na(match(c("Intercept","(Intercept)"), colnames(x))))) {
        cbind("(Intercept)"=rep.int(1, nrow(x)), x)
    } else x
}


# remove intercept column from design matrix
removeIntercept <- function(x, pos) {
    if(missing(pos)) {
        pos <- match(c("Intercept","(Intercept)"), colnames(x), nomatch = 0)
        if(any(pos > 0)) x[, -pos, drop=FALSE] else x
    } else x[, -pos, drop=FALSE]
}

# ----------------------

## utilities for plot functions

# get formula for plot functions
getFormula <- function(left, right, conditional = NULL) {
    if(is.null(conditional)) {
        as.formula(paste(left, "~", right))
    } else as.formula(paste(left, "~", right, "|", conditional))
}

# get data in the correct format for lattice graphics
# TODO: the code can be simplified by using subset() methods
getLatticeData <- function(x, ...) UseMethod("getLatticeData")
getLatticeData.cv <- function(x, select = NULL, ...) {
    CV <- as.data.frame(x$reps)
    # stack selected results on top of each other
    cvNames <- names(CV)
    cvName <- defaultCvNames(1)
    if(isTRUE(cvNames == cvName)) {
        # one column of results with default name: 
        # no column for conditional plots
        if(!is.null(select)) {
            CV <- CV[, select, drop=FALSE]
            if(ncol(CV) == 0) {
                # return data frame of NAs if column is not selected
                CV[, cvName] <- as.numeric(rep.int(NA, nrow(CV)))
            }
        }
    } else {
        # include column for conditional plots
        if(is.null(select)) {
            select <- cvNames
        } else if(!is.character(select)) select <- cvNames[select]
        n <- nrow(CV)
        if(length(select) == 0) {
            # return data frame of NAs if no results are selected
            CV <- data.frame(as.numeric(rep.int(NA, n)))
            names(CV) <- cvName
        } else {
            CV <- lapply(select, 
                function(j) data.frame(Name=rep.int(j, n), CV=CV[, j]))
            CV <- do.call(rbind, CV)
        }
    }
    # return data
    CV
}
getLatticeData.cvSelect <- function(x, subset = NULL, select = NULL, 
        reps = TRUE, numericAsFactor = FALSE, ...) {
    CV <- if(reps) x$reps else x$cv
    # extract subset of models
    if(is.null(subset)) {
#        CV$Fit <- CV$Fit[, drop=TRUE]  # drop unused factor levels
        fits <- fits(x)
    } else {
        fits <- x$cv[subset, "Fit"]
        if(reps) {
            CV <- CV[CV$Fit %in% fits, , drop=FALSE]
        } else {
            CV <- CV[subset, , drop=FALSE]
        }
    }
    # ensure that models are shown in the correct order and drop unused levels
    # ensure that correct values are shown for a numeric tuning parameter
    if(numericAsFactor && is.double(CV$Fit)) {
        CV$Fit <- factor(shingle(CV$Fit), levels=fits)
    } else if(numericAsFactor || !is.numeric(CV$Fit)) {
        CV$Fit <- factor(CV$Fit, levels=fits)
    }
    # stack selected results on top of each other
    cvNames <- names(CV)[-1]
    cvName <- defaultCvNames(1)
    if(is.null(select)) {
        select <- cvNames
    } else if(!is.character(select)) select <- cvNames[select]
    n <- nrow(CV)
    if(n == 0) {
        # no models selected: no column for grouping
        if(isTRUE(cvNames == cvName) || length(select) == 0) {
            # return data frame without column for conditional plots and one NA 
            CV <- data.frame(as.numeric(NA))
            names(CV) <- cvName
        } else {
            # return data frame with column for conditional plots and NA values
            CV <- data.frame(select, as.numeric(rep.int(NA, length(select))))
            names(CV) <- c("Name", cvName)
        }
    } else {
        # include column for grouping
        if(length(select) == 0) {
            # no results selected: no column for conditional plots and NA values
            CV <- CV[, "Fit", drop=FALSE]
            CV[, cvName] <- as.numeric(rep.int(NA, n))
        } else {
            # no column for conditional plots if there is only one column of 
            # results with default name
            if(!isTRUE(cvNames == cvName)) {
                CVFit <- CV[, "Fit", drop=FALSE]
                CV <- lapply(select, 
                    function(j) cbind(CVFit, Name=rep.int(j, n), CV=CV[, j]))
                CV <- do.call(rbind, CV)
            }
        }
    }
    # return data
    CV
}
getLatticeData.cvTuning <- function(x, ...) {
    # adjust column specifying the model in case of only one tuning parameter
    if(ncol(x$tuning) == 1) fits(x) <- x$tuning[, 1]
    # call method for class "cvSelect"
    CV <- getLatticeData.cvSelect(x, ...)
    # return data
    CV
}
