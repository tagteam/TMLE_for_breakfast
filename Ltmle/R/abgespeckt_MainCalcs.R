abgespeckt_MainCalcs <- function (inputs){
    num.final.Ynodes <- length(inputs$final.Ynodes)
    num.betas <- dim(inputs$combined.summary.measures)[2]
    n <- nrow(inputs$data)
    num.regimes <- dim(inputs$regimes)[3]
    if (inputs$verbose){ message("MainCalcs: getting weigths ...")}
    all.msm.weights <- GetMsmWeights(inputs)
    IC <- matrix(0, n, num.betas)
    IC.out = vector(num.final.Ynodes,mode = "list")
    if (inputs$verbose){ message("MainCalcs: estimating G ...")}
    g.list <- abgespeckt_EstimateG(inputs)
    ## if (inputs$verbose){ message("MainCalcs: calculating IPTW ...")}
    ## iptw <- CalcIPTW(inputs, g.list$cum.g, all.msm.weights)
    fit <- list(g = g.list$fit)
    if (inputs$verbose){ message("MainCalcs: calculating fixed time TMLE  ...")}
    beta <- vector(num.final.Ynodes,mode = "list")
    for (j in 1:num.final.Ynodes) {
        Qstar <- array(dim = c(n, num.regimes, 1))
        fixed.tmle <- do.call("FixedTimeTMLE", list(inputs,
                                                    nodes = SubsetNodes(inputs$all.nodes,final.Ynode = inputs$final.Ynodes[j]),
                                                    msm.weights = drop3(all.msm.weights[, , 1, drop = FALSE]),
                                                    combined.summary.measures = dropn(inputs$combined.summary.measures[, , , j, drop = FALSE], n = 4),
                                                    g.list = g.list))
        IC <- fixed.tmle$IC
        Qstar[, , 1] <- fixed.tmle$Qstar
        fit <- c(fit, fixed.tmle$fit)
        if (inputs$verbose){ message("MainCalcs: fitting pooled MSM  ...")}
        if (inputs$working.msm=="Y ~ -1 + S1"){
            fitted.msm=list(m.beta=mean(as.vector(Qstar)))
            n <- nrow(Qstar)
            finalIC <- matrix(Qstar[, 1, 1] - fitted.msm$m.beta,ncol = 1)
            IC <- IC + finalIC
            #C.old <- NormalizeIC(IC,combined.summary.measures=array(1,dim=c(n,1,1,1)),m.beta=array(fitted.msm$m.beta,dim=c(n,1,1)),msm.weights=all.msm.weights,observation.weights=inputs$observation.weights,g.ratio = NULL)
            C.old <- array((fitted.msm$m.beta * (1 - fitted.msm$m.beta)),dim=c(1,1))
        } else{
            fitted.msm <- FitPooledMSM(working.msm=inputs$working.msm,
                                       Qstar=Qstar,
                                       combined.summary.measures=inputs$combined.summary.measures,
                                       msm.weights=all.msm.weights * inputs$observation.weights)
            # if (j == num.final.Ynodes) browser(skipCalls = 1L)
            IC <- FinalizeIC(IC,
                             inputs$combined.summary.measures,
                             Qstar,
                             fitted.msm$m.beta,
                             all.msm.weights,
                             inputs$observation.weights,
                             inputs$id)
            C.old <- NormalizeIC(IC,
                                 inputs$combined.summary.measures,
                                 fitted.msm$m.beta,
                                 all.msm.weights,
                                 inputs$observation.weights,
                                 g.ratio = NULL)
        }
        g.ratio <- CalcGUnboundedToBoundedRatio(g.list, inputs$all.nodes,
                                                inputs$final.Ynodes)
        ## CheckForVarianceWarning(inputs, g.ratio)
        IC.j <- t(safe.solve(C.old, t(IC)))
        if (inputs$working.msm=="Y ~ -1 + S1"){
            beta.j <- lava::logit(fitted.msm$m.beta)
        } else{
            beta.j <- coef(fitted.msm$m)
        }
        names(beta.j) <- inputs$beta.names
        beta[[j]] <- beta.j
        IC.out[[j]] <- IC.j
    }
    return(list(IC = IC.out,
                msm = fitted.msm$m,
                beta = unlist(beta),
                cum.g = g.list$cum.g,
                cum.g.unbounded = g.list$cum.g.unbounded,
                fit = fit,
                variance.estimate = NULL,
                Qstar = Qstar,
                cum.g.used = fixed.tmle$cum.g.used))
}

