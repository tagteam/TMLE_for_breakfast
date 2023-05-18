MakePosDef <-
function (variance.estimate) 
{
    orig.variance.estimate <- variance.estimate
    try.result <- try({
        near.pd <- Matrix::nearPD(variance.estimate)
        variance.estimate <- as.matrix(near.pd$mat)
    }, silent = TRUE)
    if (inherits(try.result, "try-error") || !near.pd$converged || 
        any(abs(orig.variance.estimate - variance.estimate) > 
            0.001 & (abs(orig.variance.estimate - variance.estimate)/orig.variance.estimate) > 
            0.10000000000000001)) {
        warning("Covariance matrix from EstimateVariance not positive definite, unable to compute standard errors. You may want to try variance.method='ic'.")
        variance.estimate <- matrix(nrow = nrow(variance.estimate), 
            ncol = ncol(variance.estimate))
    }
    return(variance.estimate)
}
