CheckVarianceEstimateRatio <-
function (summary.obj) 
{
    if (anyNA(summary.obj$variance.estimate.ratio)) {
        warning("Unable to compute standard errors.")
        return(NULL)
    }
    if (any(summary.obj$variance.estimate.ratio > 100)) {
        warning.msg <- paste0("max(TMLE based variance estimate / IC based variance estimate) = ", 
            floor(max(summary.obj$variance.estimate.ratio)), 
            ".\nWhen this ratio is greater than 100, both variance estimates are less likely to be accurate.")
        warning(warning.msg)
    }
}
