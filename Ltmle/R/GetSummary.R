GetSummary <-
function (eff.list, cov.mat, n) 
{
    estimate <- eff.list$est
    v <- t(eff.list$gradient) %*% cov.mat %*% eff.list$gradient
    stopifnot(length(v) == 1)
    std.dev <- sqrt(v[1, 1]/n)
    if (eff.list$log.std.err) {
        pvalue <- 2 * GetPValue(-abs(log(estimate)/std.dev), 
            n)
        CI <- exp(GetCI(log(estimate), std.dev, n))
    }
    else {
        pvalue <- 2 * GetPValue(-abs(estimate/std.dev), n)
        CI <- GetCI(estimate, std.dev, n)
    }
    CI <- Bound(CI, eff.list$CIBounds)
    return(list(long.name = eff.list$long.name, estimate = estimate, 
        std.dev = std.dev, pvalue = pvalue, CI = CI, log.std.err = eff.list$log.std.err))
}
