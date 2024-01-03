GetMsmWeights <- function (inputs) {
    n <- nrow(inputs$data)
    num.regimes <- dim(inputs$regimes)[3]
    stopifnot(num.regimes >= 1)
    num.final.Ynodes <- length(inputs$final.Ynodes)
    if (is.equal(inputs$msm.weights, "empirical")) {
        msm.weights <- matrix(nrow = num.regimes, ncol = num.final.Ynodes)
        for (j in 1:num.final.Ynodes) {
            final.Ynode <- inputs$final.Ynodes[j]
            regimes.subset <- inputs$regimes[, inputs$all.nodes$A < 
                final.Ynode, , drop = FALSE]
            if (ncol(regimes.subset) > 0) {
                is.duplicate <- duplicated(regimes.subset, MARGIN = 3)
            }
            else {
                is.duplicate <- c(FALSE, rep(TRUE, num.regimes - 
                  1))
            }
            uncensored <- IsUncensored(inputs$uncensored, inputs$all.nodes$C, 
                cur.node = final.Ynode)
            intervention.match <- InterventionMatch(inputs$intervention.match, 
                inputs$all.nodes$A, cur.node = final.Ynode)
            for (i in 1:num.regimes) {
                if (is.duplicate[i]) {
                  msm.weights[i, j] <- 0
                }
                else {
                  msm.weights[i, j] <- sum(uncensored & intervention.match[, 
                    i])/nrow(inputs$data)
                }
            }
        }
    }
    else if (is.null(inputs$msm.weights)) {
        msm.weights <- array(1, dim = c(n, num.regimes, num.final.Ynodes))
    }
    else {
        msm.weights <- inputs$msm.weights
    }
    if (is.equal(dim(msm.weights), c(num.regimes, num.final.Ynodes))) {
        msm.weights <- array(rep(msm.weights, each = n), dim = c(n, 
            num.regimes, num.final.Ynodes))
    }
    if (anyNA(msm.weights) || any(msm.weights < 0)) 
        stop("all msm.weights must be >= 0 and not NA")
    return(msm.weights)
}
