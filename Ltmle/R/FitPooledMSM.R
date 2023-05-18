FitPooledMSM <-
function (working.msm, Qstar, combined.summary.measures, msm.weights) 
{
    n <- dim(Qstar)[1]
    num.regimes <- dim(Qstar)[2]
    num.final.Ynodes <- dim(Qstar)[3]
    num.summary.measures <- dim(combined.summary.measures)[2]
    X <- apply(combined.summary.measures, 2, rbind)
    Y <- as.vector(Qstar)
    weight.vec <- as.vector(msm.weights)
    data.pooled <- data.frame(Y, X)
    positive.weight <- weight.vec > 0
    m <- ltmle.glm(formula(working.msm), data = data.pooled[positive.weight, 
        ], family = quasibinomial(), weights = weight.vec[positive.weight])
    SuppressGivenWarnings(m.beta <- predict(m, newdata = data.pooled, 
        type = "response"), "prediction from a rank-deficient fit may be misleading")
    dim(m.beta) <- dim(Qstar)
    return(list(m = m, m.beta = m.beta))
}
