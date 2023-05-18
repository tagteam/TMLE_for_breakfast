summary.ltmleMSM <-
function (object, estimator = ifelse(object$gcomp, "gcomp", "tmle"), 
    ...) 
{
    info <- GetSummaryLtmleMSMInfo(object, estimator)
    estimate <- info$estimate
    v <- info$v
    n <- nrow(info$IC)
    std.dev <- sqrt(v/n)
    pval <- 2 * GetPValue(-abs(estimate/std.dev), n)
    CI <- GetCI(estimate, std.dev, n)
    cmat <- cbind(estimate, std.dev, CI, pval)
    dimnames(cmat) <- list(names(estimate), c("Estimate", "Std. Error", 
        "CI 2.5%", "CI 97.5%", "p-value"))
    ans <- list(cmat = cmat, estimator = estimator, transformOutcome = object$transformOutcome, 
        variance.estimate.ratio = info$variance.estimate.ratio)
    class(ans) <- "summary.ltmleMSM"
    return(ans)
}
