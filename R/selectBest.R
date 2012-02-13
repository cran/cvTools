# ----------------------
# Author: Andreas Alfons
#         KU Leuven
# ----------------------

# Model selection criteria based on prediction loss
# 
# Determine the best model based on prediction loss.
# 
# \code{selectMin} selects the model with the smallest prediction loss.  
# 
# \code{selectHastie} is useful for nested models or for models where a 
# tuning parameter controls the complexity of the model (e.g., penalized 
# regression).  It selects the most parsimonious model whose prediction error 
# is no larger than \code{sdFactor} standard errors above the prediction error 
# of the best model.  In particular a one-standard-error rule is frequently 
# applied.
# 
# @rdname selectBest
# @name selectBest
# 
# @param x  a numeric vector containing the estimated prediction errors for 
# the models.
# @param sd  a numeric vector containing the estimated standard errors of the 
# prediction loss for the models.
# @param sdFactor  a numeric value giving the multiplication factor of the 
# standard error.
# 
# @return An integer giving the index of the best model.
# 
# @note These functions are designed to be supplied to \code{\link{cvSelect}} 
# or \code{\link{cvTuning}}.
# 
# @author Andreas Alfons
# 
# @references 
# Hastie, T., Tibshirani, R. and Friedman, J. (2009) \emph{The Elements of 
# Statistical Learning: Data Mining, Inference, and Prediction}.  Springer, 
# 2nd edition.
# 
# @seealso \code{\link{cvSelect}}, \code{\link{cvTuning}}
# 
# @keywords utilities
# 
# NULL
# 
# @rdname selectBest
# @export
selectMin <- function(x) which.min(x)

# @rdname selectBest
# @export
selectHastie <- function(x, sd, sdFactor = 1) {
    i <- which.min(x)
    within <- which(x[seq_len(i)] < (x[i] + sdFactor * sd[i]))
    if(length(within) == 0) i else within[1]  # ensure it works if sd[i] is NA
}

#selectDiff <- function(x, sd, sdFactor = 1) {
#    i <- which.min(x)
#    seqI <- seq_len(i)
#    out <- which(c(TRUE, (diff(x[seqI]) + sdFactor * sd[seqI[-1]]) < 0))
#    tail(out, 1)
#}
#
#selectRelChange <- function(x, sd, sdFactor = 1, threshold = 0.001) {
#    # find models with large enough relative change
#    i <- which.min(x)
#    xSeqI <- x[seq_len(i)]
#    keep <- which((xSeqI - x[i]) / max(xSeqI) > threshold)
#    # call selectHastie() for the remaining models
#    selectHastie(xSeqI[keep], sd[keep], sdFactor=sdFactor)
#}
