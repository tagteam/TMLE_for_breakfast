NormalizeIC <-
function (IC, combined.summary.measures, m.beta, msm.weights, 
    observation.weights, g.ratio) 
{
    n <- dim(combined.summary.measures)[1]
    num.betas <- dim(combined.summary.measures)[2]
    num.regimes <- dim(combined.summary.measures)[3]
    num.final.Ynodes <- dim(combined.summary.measures)[4]
    if (is.null(g.ratio)) {
        g.ratio <- array(1, dim = c(n, num.regimes, num.final.Ynodes))
    }
    C <- array(0, dim = c(num.betas, num.betas))
    for (j in 1:num.final.Ynodes) {
        for (i in 1:num.regimes) {
            tempC <- crossprod(combined.summary.measures[, , 
                i, j] * g.ratio[, i, j], combined.summary.measures[, 
                , i, j] * g.ratio[, i, j] * msm.weights[, i, 
                j] * m.beta[, i, j] * (1 - m.beta[, i, j]) * 
                observation.weights)
            if (anyNA(tempC)) 
                stop("NA in tempC")
            C <- C + tempC
        }
    }
    C <- C/n
    if (rcond(C) < 9.9999999999999998e-13) {
        C <- matrix(NA, nrow = num.betas, ncol = num.betas)
        warning("rcond(C) near 0, standard errors not available")
    }
    return(C)
}
