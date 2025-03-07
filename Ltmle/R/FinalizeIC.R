FinalizeIC <- function(IC,
                       combined.summary.measures,
                       Qstar,
                       m.beta,
                       msm.weights,
                       observation.weights,
                       id){
    num.betas <- ncol(IC)
    n <- nrow(Qstar)
    num.regimes <- ncol(Qstar)
    num.final.Ynodes <- dim(Qstar)[3]
    stopifnot(num.betas == ncol(combined.summary.measures))
    finalIC <- matrix(0, nrow = n, ncol = num.betas)
    for (i in 1:num.regimes) {
        # hack: iterative regressions for multiple outcomes msm.weights[, i, 1] instead of msm.weights[, i, j]
        if (any(msm.weights[, i, 1] > 0)) {
            m1 <- matrix(Qstar[, i, 1] - m.beta[, i, 1],ncol = 1)
            for (k in 1:num.betas) {
                m2 <- combined.summary.measures[, k, i, 1]
                # hack: iterative regressions for multiple outcomes msm.weights[, i, 1] instead of msm.weights[, i, j]
                finalIC[, k] <- finalIC[, k] + msm.weights[, i, 1] * observation.weights * (m1 * m2)
            }
        }
    }
    IC <- IC + finalIC
    return(HouseholdIC(IC, id))
}
