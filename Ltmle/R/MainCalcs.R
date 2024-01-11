MainCalcs <- function (inputs){
    num.final.Ynodes <- length(inputs$final.Ynodes)
    num.betas <- dim(inputs$combined.summary.measures)[2]
    n <- nrow(inputs$data)
    num.regimes <- dim(inputs$regimes)[3]
    Qstar <- array(dim = c(n, num.regimes, num.final.Ynodes))
    if (inputs$verbose){ message("MainCalcs: getting weigths ...")}
    all.msm.weights <- GetMsmWeights(inputs)
    new.var.y <- array(dim = c(num.betas, num.betas, num.final.Ynodes))
    IC <- matrix(0, n, num.betas)
    IC.y <- array(dim = c(n, num.betas, num.final.Ynodes))
    if (inputs$verbose){ message("MainCalcs: estimating G ...")}
    g.list <- EstimateG(inputs)
    if (inputs$verbose){ message("MainCalcs: calculating IPTW ...")}
    fit <- list(g = g.list$fit)
    if (inputs$iptw.only) {
        iptw <- CalcIPTW(inputs, g.list$cum.g, all.msm.weights)
        beta <- rep(NA, length(iptw$beta))
        fitted.msm <- NULL
        variance.estimate <- NULL
        fixed.tmle <- list(cum.g.used = array(NA, dim = dim(g.list$cum.g)))
        iptw.ic <- iptw$IC
        iptw.beta <- iptw$beta
    }
    else {
        if (inputs$verbose){ message("MainCalcs: calculating fixed time TMLE  ...")}
        for (j in 1:num.final.Ynodes) {
            fixed.tmle <- do.call("FixedTimeTMLE",list(inputs, nodes = SubsetNodes(inputs$all.nodes,
                final.Ynode = inputs$final.Ynodes[j]), msm.weights = drop3(all.msm.weights[,
                , j, drop = FALSE]), combined.summary.measures = dropn(inputs$combined.summary.measures[,
                , , j, drop = FALSE], n = 4), g.list = g.list))
            IC <- IC + fixed.tmle$IC
            IC.y[, , j] <- fixed.tmle$IC
            Qstar[, , j] <- fixed.tmle$Qstar
            new.var.y[, , j] <- fixed.tmle$est.var
        }
        fit <- c(fit, fixed.tmle$fit)
        
        if (isTRUE(attr(inputs$data, "called.from.estimate.variance",
            exact = TRUE))) {
            return(list(IC = matrix(NA, 1, 1), msm = NULL, beta = qlogis(mean(Qstar)),
                cum.g = g.list$cum.g, cum.g.unbounded = g.list$cum.g.unbounded,
                fit = fit, variance.estimate = NULL, beta.iptw = NULL,
                IC.iptw = NULL, Qstar = Qstar, cum.g.used = fixed.tmle$cum.g.used))
        }
        if (inputs$verbose){ message("MainCalcs: fitting pooled MSM  ...")}
        fitted.msm <- FitPooledMSM(inputs$working.msm, Qstar,
            inputs$combined.summary.measures, all.msm.weights *
                inputs$observation.weights)
        IC <- FinalizeIC(IC, inputs$combined.summary.measures,
            Qstar, fitted.msm$m.beta, all.msm.weights, inputs$observation.weights,
            inputs$id)
        C.old <- NormalizeIC(IC, inputs$combined.summary.measures,
            fitted.msm$m.beta, all.msm.weights, inputs$observation.weights,
            g.ratio = NULL)
        g.ratio <- CalcGUnboundedToBoundedRatio(g.list, inputs$all.nodes,
            inputs$final.Ynodes)
        CheckForVarianceWarning(inputs, g.ratio)
        if (inputs$variance.method == "ic") {
            variance.estimate <- NULL
        }
        else {
            new.var <- matrix(NA, num.betas, num.betas)
            for (i in 1:num.betas) {
                for (j in 1:num.betas) {
                  if (num.final.Ynodes > 1) {
                    cov.IC <- cov(IC.y[, i, ], IC.y[, j, ])
                    diag(cov.IC) <- new.var.y[i, j, ]
                    new.var[i, j] <- sum(cov.IC)
                  }
                  else {
                    new.var[i, j] <- new.var.y[i, j, 1]
                  }
                }
            }
            C <- NormalizeIC(IC, inputs$combined.summary.measures,
                fitted.msm$m.beta, all.msm.weights, inputs$observation.weights,
                g.ratio)
            variance.estimate <- safe.solve(C) %*% new.var %*%
                t(safe.solve(C))
        }
        IC <- t(safe.solve(C.old, t(IC)))
        beta <- coef(fitted.msm$m)
        names(beta) <- inputs$beta.names
        iptw.ic <- NULL
        iptw.beta <- NULL
        
    }
    return(list(IC = IC, msm = fitted.msm$m, beta = beta, cum.g = g.list$cum.g,
        cum.g.unbounded = g.list$cum.g.unbounded, fit = fit,
        variance.estimate = variance.estimate, beta.iptw = iptw.beta,
        IC.iptw = iptw.ic, Qstar = Qstar, cum.g.used = fixed.tmle$cum.g.used))
}
