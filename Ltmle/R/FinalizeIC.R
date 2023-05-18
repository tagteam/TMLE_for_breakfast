FinalizeIC <-
function (IC, combined.summary.measures, Qstar, m.beta, msm.weights, 
    observation.weights, id) 
{
    num.betas <- ncol(IC)
    n <- nrow(Qstar)
    num.regimes <- ncol(Qstar)
    num.final.Ynodes <- dim(Qstar)[3]
    stopifnot(num.betas == ncol(combined.summary.measures))
    finalIC <- matrix(0, nrow = n, ncol = num.betas)
    for (j in 1:num.final.Ynodes) {
        for (i in 1:num.regimes) {
            if (any(msm.weights[, i, j] > 0)) {
                m1 <- matrix(Qstar[, i, j] - m.beta[, i, j], 
                  ncol = 1)
                for (k in 1:num.betas) {
                  m2 <- combined.summary.measures[, k, i, j]
                  finalIC[, k] <- finalIC[, k] + msm.weights[, 
                    i, j] * observation.weights * (m1 * m2)
                }
            }
        }
    }
    IC <- IC + finalIC
    return(HouseholdIC(IC, id))
}
