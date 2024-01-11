FixedTimeTMLE <- function(inputs, nodes, msm.weights, combined.summary.measures, g.list){
    data <- inputs$data
    num.regimes <- dim(inputs$regimes)[3]
    n <- nrow(data)
    num.betas <- ncol(combined.summary.measures)
    IC <- matrix(0, nrow = n, ncol = num.betas)
    cum.g.used <- array(FALSE, dim = dim(g.list$cum.g))
    est.var <- matrix(0, num.betas, num.betas)
    regimes.with.positive.weight <- which(apply(msm.weights > 0, 2, any))
    if (length(regimes.with.positive.weight) == 0)
        stop("All regimes have weight 0 (one possible reason is that msm.weights='empirical' and no data rows match any of the regimes and are uncensored)")
    fit.Qstar <- fit.Q <- vector("list", length(nodes$LY))
    names(fit.Qstar) <- names(fit.Q) <- names(data)[nodes$LY]
    Qstar.kplus1 <- matrix(data[, max(nodes$Y)], nrow = n, ncol = num.regimes)
    mean.summary.measures <- apply(abs(combined.summary.measures), 2, mean)
    if (length(nodes$LY) > 0) {
        for (LYnode.index in rev(seq_along(nodes$LY))) {
            cur.node <- nodes$LY[LYnode.index]
            deterministic.list.origdata <- IsDeterministic(data,
                                                           cur.node, inputs$deterministic.Q.function, nodes,
                                                           called.from.estimate.g = FALSE, inputs$survivalOutcome)
            uncensored <- IsUncensored(inputs$uncensored, nodes$C,
                                       cur.node)
            intervention.match <- InterventionMatch(inputs$intervention.match,
                                                    nodes$A, cur.node)
            if (inputs$stratify) {
                subs <- uncensored & intervention.match & !deterministic.list.origdata$is.deterministic
            }
            else {
                subs <- uncensored & !deterministic.list.origdata$is.deterministic
            }
            if (inputs$verbose){ message("FixedTimeTMLE: estimating Q for ",names(data)[cur.node]," using ", sum(subs)," observations.")}
            Q.est <- Estimate(inputs, form = inputs$Qform[LYnode.index],
                              Qstar.kplus1 = if (LYnode.index == length(nodes$LY))
                                                 Qstar.kplus1[, 1]
                                             else Qstar.kplus1, family = quasibinomial(),
                              subs = subs, type = "link", nodes = nodes, called.from.estimate.g = FALSE,
                              calc.meanL = FALSE, cur.node = cur.node, regimes.meanL = NULL,
                              regimes.with.positive.weight = regimes.with.positive.weight)
            logitQ <- Q.est$predicted.values
            fit.Q[[LYnode.index]] <- Q.est$fit
            ACnode.index <- which.max(nodes$AC[nodes$AC < cur.node])
            SuppressGivenWarnings(update.list <- UpdateQ(Qstar.kplus1 = Qstar.kplus1,
                                                         logitQ = logitQ,
                                                         combined.summary.measures = combined.summary.measures,
                                                         cum.g = g.list$cum.g[, ACnode.index, ],
                                                         working.msm = inputs$working.msm,
                                                         uncensored = uncensored,
                                                         intervention.match = intervention.match,
                                                         is.deterministic = deterministic.list.origdata$is.deterministic,
                                                         msm.weights = msm.weights,
                                                         gcomp = inputs$gcomp,
                                                         observation.weights = inputs$observation.weights),
                                  GetWarningsToSuppress(update.step = TRUE))
            if (length(ACnode.index) > 0)
                cum.g.used[, ACnode.index, ] <- cum.g.used[, ACnode.index, ] | update.list$cum.g.used
            Qstar <- update.list$Qstar
            Qstar[Q.est$is.deterministic] <- Q.est$deterministic.Q[Q.est$is.deterministic]
            curIC <- CalcIC(Qstar.kplus1, Qstar, update.list$h.g.ratio,
                            uncensored, intervention.match, regimes.with.positive.weight)
            curIC.relative.error <- abs(colSums(curIC))
            curIC.relative.error[mean.summary.measures > 0] <- curIC.relative.error[mean.summary.measures > 0]/mean.summary.measures[mean.summary.measures > 0]
            if (any(curIC.relative.error > 0.001) && !inputs$gcomp) {
                if (inputs$verbose) message("Calling FixScoreEquation")
                SetSeedIfRegressionTesting()
                fix.score.list <- FixScoreEquation(Qstar.kplus1,
                                                   update.list$h.g.ratio, uncensored, intervention.match,
                                                   Q.est$is.deterministic, Q.est$deterministic.Q,
                                                   update.list$off, update.list$X, regimes.with.positive.weight)
                Qstar <- fix.score.list$Qstar
                curIC <- CalcIC(Qstar.kplus1, Qstar, update.list$h.g.ratio,
                                uncensored, intervention.match, regimes.with.positive.weight)
                update.list$fit <- fix.score.list$fit
            }
            est.var <- est.var + EstimateVariance(inputs, nodes,
                                                  combined.summary.measures, regimes.with.positive.weight,
                                                  uncensored, alive = !deterministic.list.origdata$is.deterministic,
                                                  Qstar, Qstar.kplus1, cur.node, msm.weights, LYnode.index,
                                                  ACnode.index, g.list$cum.g, g.list$prob.A.is.1,
                                                  g.list$cum.g.meanL, g.list$cum.g.unbounded, g.list$cum.g.meanL.unbounded,
                                                  inputs$observation.weights, is.last.LYnode = (LYnode.index ==
                                                                                                length(nodes$LY)), intervention.match)
            IC <- IC + curIC
            Qstar.kplus1 <- Qstar
            fit.Qstar[[LYnode.index]] <- update.list$fit
        }
    }
    else {
        Qstar <- Qstar.kplus1
    }
    return(list(IC = IC, Qstar = Qstar, est.var = est.var, fit = list(Q = ReorderFits(fit.Q),
                                                                      Qstar = fit.Qstar), cum.g.used = cum.g.used))
}
