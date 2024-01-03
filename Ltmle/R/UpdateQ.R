UpdateQ <- function(Qstar.kplus1,
                    logitQ,
                    combined.summary.measures,
                    cum.g, 
                    working.msm,
                    uncensored,
                    intervention.match,
                    is.deterministic, 
                    msm.weights,
                    gcomp,
                    observation.weights) {
    n <- nrow(logitQ)
    num.regimes <- ncol(logitQ)
    off <- as.vector(logitQ)
    Y <- as.vector(Qstar.kplus1)
    if (length(cum.g) == 0) 
        cum.g <- 1
    stacked.summary.measures <- apply(combined.summary.measures, 
        2, rbind)
    subs.vec <- uncensored & !is.deterministic & as.vector(intervention.match)
    weight.vec <- numeric(n * num.regimes)
    weight.vec[subs.vec] <- (observation.weights * as.vector(msm.weights)/as.vector(cum.g))[subs.vec]
    if (anyNA(weight.vec)) 
        stop("NA in weight.vec")
    f <- as.formula(paste(working.msm, "+ offset(off)"))
    data.temp <- data.frame(Y, stacked.summary.measures, off)
    if (gcomp) {
        Qstar <- plogis(logitQ)
        m <- "no Qstar fit because gcomp=TRUE (so no updating step)"
    }
    else {
      # browser()
        if (any(weight.vec > 0)) {
            m <- ltmle.glm(f, data = data.temp[weight.vec > 0, ], family = quasibinomial(),
                           weights = as.vector(scale(weight.vec[weight.vec > 0], center = FALSE)))
            Qstar <- matrix(predict(m, newdata = data.temp, type = "response"), 
                nrow = nrow(logitQ))
        }
        else {
            Qstar <- plogis(logitQ)
            m <- "no Qstar fit because no subjects alive, uncensored, following intervention"
        }
    }
    indicator <- matrix(uncensored * observation.weights, nrow = nrow(stacked.summary.measures), 
        ncol = ncol(stacked.summary.measures)) * matrix(intervention.match, 
        nrow = nrow(stacked.summary.measures), ncol = ncol(stacked.summary.measures))
    h.g.ratio <- stacked.summary.measures/matrix(cum.g, nrow = nrow(stacked.summary.measures), 
        ncol = ncol(stacked.summary.measures)) * indicator
    dim(h.g.ratio) <- c(n, num.regimes, ncol(h.g.ratio))
    for (i in 1:num.regimes) {
        h.g.ratio[, i, ] <- h.g.ratio[, i, ] * msm.weights[, 
            i]
        weight.zero.index <- msm.weights[, i] == 0
        h.g.ratio[weight.zero.index, i, ] <- 0
    }
    cum.g.used <- weight.vec > 0 & msm.weights > 0
    dim(cum.g.used) <- c(n, num.regimes)
    return(list(Qstar = Qstar, h.g.ratio = h.g.ratio, X = stacked.summary.measures, 
        off = off, fit = m, cum.g.used = cum.g.used))
}
