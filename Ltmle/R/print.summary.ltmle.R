print.summary.ltmle <-
function (x, ...) 
{
    cat("Estimator: ", x$estimator, "\n")
    if (x$estimator == "gcomp") {
        cat("Warning: inference for gcomp is not accurate! It is based on TMLE influence curves.\n")
    }
    PrintCall(x$call)
    PrintSummary(x$treatment)
    CheckVarianceEstimateRatio(x)
    invisible(x)
}
