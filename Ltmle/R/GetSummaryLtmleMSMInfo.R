GetSummaryLtmleMSMInfo <-
function (object, estimator) 
{
    if (!estimator %in% c("tmle", "iptw", "gcomp")) 
        stop("estimator should be one of: tmle, iptw, gcomp")
    if (estimator == "tmle") {
        if (object$gcomp) 
            stop("estimator 'tmle' is not available because ltmleMSM was called with gcomp=TRUE")
        estimate <- object$beta
        IC <- object$IC
    }
    else if (estimator == "iptw") {
        estimate <- object$beta.iptw
        IC <- object$IC.iptw
    }
    else if (estimator == "gcomp") {
        if (!object$gcomp) 
            stop("estimator 'gcomp' is not available because ltmleMSM was called with gcomp=FALSE")
        estimate <- object$beta
        IC <- object$IC
    }
    IC.variance <- apply(IC, 2, var)
    if (is.null(object$variance.estimate)) {
        v <- IC.variance
    }
    else {
        v <- pmax(diag(object$variance.estimate), IC.variance)
    }
    variance.estimate.ratio <- v/IC.variance
    return(list(estimate = estimate, IC = IC, variance.estimate.ratio = variance.estimate.ratio, 
        v = v))
}
