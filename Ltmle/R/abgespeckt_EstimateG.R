abgespeckt_EstimateG <- function (inputs){
    n <- nrow(inputs$data)
    num.regimes <- dim(inputs$regimes)[3]
    nodes <- inputs$all.nodes
    g <- cum.g <- cum.g.unbounded <- prob.A.is.1 <- array(NaN,dim = c(n, length(nodes$AC), num.regimes))
    cum.g.meanL <- cum.g.meanL.unbounded <- NULL
    fit <- vector("list", length(nodes$AC))
    names(fit) <- names(inputs$data)[nodes$AC]
    for (i in seq_along(nodes$AC)) {
        cur.node <- nodes$AC[i]
        uncensored <- IsUncensored(inputs$uncensored,nodes$C,cur.node)
        deterministic.origdata <- IsDeterministic(inputs$data,
                                                  cur.node, inputs$deterministic.Q.function, nodes,
                                                  called.from.estimate.g = TRUE, inputs$survivalOutcome)$is.deterministic
        form <- inputs$gform[i]
        deterministic.g.list.origdata <- IsDeterministicG(inputs$data,cur.node,inputs$deterministic.g.function,nodes,using.newdata = FALSE)
        deterministic.g.origdata <- deterministic.g.list.origdata$is.deterministic
        subs <- uncensored & !deterministic.origdata & !deterministic.g.origdata
        g.est <- Estimate(inputs, form = form, Qstar.kplus1 = NULL,
                          subs = subs, family = quasibinomial(), type = "response",
                          nodes = nodes, called.from.estimate.g = TRUE,
                          calc.meanL = FALSE,
                          cur.node = cur.node, regimes.meanL = NULL,
                          regimes.with.positive.weight = 1:num.regimes)
        prob.A.is.1[, i, ] <- g.est$predicted.values
        fit[[i]] <- g.est$fit
        if (cur.node %in% nodes$A) {
            cur.abar <- AsMatrix(inputs$regimes[, nodes$A == cur.node, ])
            cur.abar.meanL <- cur.abar
        }
        else {
            cur.abar <- cur.abar.meanL <- matrix(1, nrow(inputs$data), num.regimes)
        }
        g[, i, ] <- CalcG(AsMatrix(prob.A.is.1[, i, ]),
                          cur.abar,
                          g.est$is.deterministic)
    }
    for (regime.index in 1:num.regimes) {
        cum.g.list <- CalcCumG(AsMatrix(g[, , regime.index]), inputs$gbounds)
        cum.g[, , regime.index] <- cum.g.list$bounded
        cum.g.unbounded[, , regime.index] <- cum.g.list$unbounded
    }
    ## print(cum.g)
    return(list(cum.g = cum.g, cum.g.unbounded = cum.g.unbounded,
                cum.g.meanL = cum.g.meanL, fit = ReorderFits(fit), prob.A.is.1 = prob.A.is.1,
                cum.g.meanL.unbounded = cum.g.meanL.unbounded))
}
