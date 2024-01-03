CalcIPTW <-
function (inputs, cum.g, msm.weights) 
{
    if (isTRUE(attr(inputs$data, "called.from.estimate.variance", 
        exact = TRUE))) {
        return(list(beta = NA, IC = matrix(NA, 1, 1)))
    }
    nodes <- inputs$all.nodes
    n <- nrow(inputs$data)
    num.regimes <- dim(inputs$regimes)[3]
    num.final.Ynodes <- length(inputs$final.Ynodes)
    Y.vec <- X.mat <- weight.vec <- NULL
    save.xy <- list()
    for (j in 1:num.final.Ynodes) {
        final.Ynode <- inputs$final.Ynodes[j]
        intervention.match <- InterventionMatch(inputs$intervention.match, 
            nodes$A, cur.node = final.Ynode)
        uncensored <- IsUncensored(inputs$uncensored, nodes$C, 
            final.Ynode)
        for (i in 1:num.regimes) {
            index <- uncensored & intervention.match[, i]
            col.index <- which.max(nodes$AC[nodes$AC < final.Ynode])
            Y <- inputs$data[index, final.Ynode]
            if (length(col.index > 0)) {
                g <- cum.g[index, col.index, i]
            }
            else {
                g <- 1
            }
            X <- inputs$combined.summary.measures[index, , i, 
                j]
            if (is.vector(X)) {
                dim(X) <- c(sum(index), ncol(inputs$combined.summary.measures))
            }
            weight <- msm.weights[index, i, j] * inputs$observation.weights[index]/g
            weight[msm.weights[index, i, j] == 0 | inputs$observation.weights[index] == 
                0] <- 0
            save.xy[[length(save.xy) + 1]] <- list(X = X, Y = Y, 
                weight = weight, index = index)
            Y.vec <- c(Y.vec, Y)
            X.mat <- rbind(X.mat, X)
            weight.vec <- c(weight.vec, weight)
        }
    }
    colnames(X.mat) <- colnames(inputs$combined.summary.measures)
    if (nrow(X.mat) == 0) {
        warning("no rows uncensored and matching regimes/abar - IPTW returns NA")
        num.beta <- ncol(inputs$combined.summary.measures)
        return(list(beta = rep(NA, num.beta), IC = matrix(nrow = n, 
            ncol = num.beta)))
    }
    # browser()
    m.glm <- ltmle.glm(formula(inputs$working.msm), family = quasibinomial(), 
        data = data.frame(Y = Y.vec, X.mat, weight.vec), weights = as.vector(scale(weight.vec, 
            center = FALSE)))
    beta <- coef(m.glm)
    IC <- matrix(0, nrow = n, ncol = length(beta))
    m.beta <- array(dim = c(n, num.regimes, num.final.Ynodes))
    cnt <- 0
    for (j in 1:num.final.Ynodes) {
        final.Ynode <- inputs$final.Ynodes[j]
        for (i in 1:num.regimes) {
            newdata <- data.frame(inputs$combined.summary.measures[, 
                , i, j])
            colnames(newdata) <- colnames(inputs$combined.summary.measures)
            SuppressGivenWarnings(m.beta[, i, j] <- predict(m.glm, 
                newdata = newdata, type = "response"), "prediction from a rank-deficient fit may be misleading")
            cnt <- cnt + 1
            XY.list <- save.xy[[cnt]]
            IC[XY.list$index, ] <- IC[XY.list$index, ] + XY.list$weight * 
                XY.list$X * (XY.list$Y - m.beta[XY.list$index, 
                i, j])
        }
    }
    C <- NormalizeIC(IC, inputs$combined.summary.measures, m.beta, 
        msm.weights, observation.weights = inputs$observation.weights, 
        g.ratio = NULL)
    normalized.IC <- t(safe.solve(C, t(IC)))
    household.IC <- HouseholdIC(normalized.IC, inputs$id)
    names(beta) <- inputs$beta.names
    return(list(beta = beta, IC = household.IC))
}
