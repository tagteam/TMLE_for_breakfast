print.summary.ltmleEffectMeasures <-
function (x, ...) 
{
    cat("Estimator: ", x$estimator, "\n")
    if (x$estimator == "gcomp") {
        cat("Warning: inference for gcomp is not accurate! It is based on TMLE influence curves.\n")
    }
    PrintCall(x$call)
    lapply(x$effect.measures, PrintSummary)
    CheckVarianceEstimateRatio(x)
    invisible(x)
}
