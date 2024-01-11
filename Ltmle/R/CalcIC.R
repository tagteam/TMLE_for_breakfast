CalcIC <- function (Qstar.kplus1, Qstar, h.g.ratio, uncensored, intervention.match, 
                    regimes.with.positive.weight) 
{
    n <- nrow(Qstar)
    num.betas <- dim(h.g.ratio)[3]
    IC <- matrix(0, nrow = n, ncol = num.betas)
    for (i in regimes.with.positive.weight) {
        index <- uncensored & intervention.match[, i]
        if (any(h.g.ratio[index, i, ] != 0)) {
            IC[index, ] <- IC[index, ] + (Qstar.kplus1[index, i] - Qstar[index, i]) * h.g.ratio[index, i, ]
        }
    }
    return(IC)
}
