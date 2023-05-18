FixScoreEquation <-
function (Qstar.kplus1, h.g.ratio, uncensored, intervention.match, 
    is.deterministic, deterministic.Q, off, X, regimes.with.positive.weight) 
{
    CalcScore <- function(e) {
        Qstar <- QstarFromE(e)
        ICtemp <- CalcIC(Qstar.kplus1, Qstar, h.g.ratio, uncensored, 
            intervention.match, regimes.with.positive.weight)
        return(sum(colSums(ICtemp)^2))
    }
    QstarFromE <- function(e) {
        Qstar <- plogis(off + X %*% e)
        dim(Qstar) <- dim(Qstar.kplus1)
        Qstar[is.deterministic] <- deterministic.Q[is.deterministic]
        return(Qstar)
    }
    FindMin <- function(minimizer) {
        num.tries <- 20
        init.e <- numeric(num.betas)
        for (i in 1:num.tries) {
            m <- nlminb(start = init.e, objective = CalcScore, 
                control = list(abs.tol = max.objective, eval.max = 500, 
                  iter.max = 500, x.tol = 1e-14, rel.tol = 1e-14))
            e <- m$par
            obj.val <- m$objective
            if (obj.val < max.objective) {
                m$ltmle.msg <- "updating step using glm failed to solve score equation; solved using nlminb"
                return(list(e = e, solved = TRUE, m = m))
            }
            init.e <- rnorm(num.betas)
        }
        return(list(e = numeric(num.betas), solved = FALSE, m = "score equation not solved!"))
    }
    max.objective <- 0.0001^2
    num.betas <- ncol(X)
    for (offset.lbound in c(1e-08, 0.0001, 0.001, 0.01)) {
        off <- Bound(off, qlogis(c(offset.lbound, 1 - offset.lbound)))
        l <- FindMin("nlminb")
        if (l$solved) 
            break
    }
    if (!l$solved) 
        stop("minimizer failed")
    Qstar <- QstarFromE(l$e)
    return(list(Qstar = Qstar, fit = l$m))
}
