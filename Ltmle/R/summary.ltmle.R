summary.ltmle <-
function (object, estimator = ifelse(object$gcomp, "gcomp", "tmle"),
    ...)
{
    if ("control.object" %in% names(list(...)))
        stop("control.object has been deprecated. To obtain additive treatment effect, risk ratio, and relative risk, call ltmle with abar=list(treatment, control). See ?ltmle and ?summary.ltmleEffectMeasures.")
    if (!estimator[1] %in% c("tmle", "iptw", "gcomp"))
        stop("estimator should be one of: tmle, iptw, gcomp. If you are trying to use control.object, the control.object parameter has been deprecated. To obtain additive treatment effect, risk ratio, and relative risk, call ltmle with abar=list(treatment, control). See ?ltmle and ?summary.ltmleEffectMeasures.")
    if (estimator == "tmle" && object$gcomp)
        stop("estimator 'tmle' is not available because ltmleMSM was called with gcomp=TRUE")
    if (estimator == "gcomp" && !object$gcomp)
        stop("estimator 'gcomp' is not available because ltmleMSM was called with gcomp=FALSE")
    IC.variance <- var(object$IC[[estimator]])
    if (estimator == "tmle" && !is.null(object$variance.estimate)) {
        v <- max(IC.variance, object$variance.estimate)
    }
    else {
        v <- IC.variance
    }
    variance.estimate.ratio = v/IC.variance
    if (object$binaryOutcome) {
        CIBounds <- c(0, 1)
    }
    else {
        CIBounds <- c(-Inf, Inf)
    }
    treatment <- GetSummary(list(long.name = NULL, est = object$estimates[estimator],
                                 gradient = 1, log.std.err = FALSE, CIBounds = CIBounds),
                            v,
                            n = length(object$IC[[estimator]]))
    ans <- list(treatment = treatment, call = object$call, estimator = estimator,
        variance.estimate.ratio = variance.estimate.ratio)
    class(ans) <- "summary.ltmle"
    return(ans)
}
