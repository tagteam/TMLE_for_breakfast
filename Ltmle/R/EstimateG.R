EstimateG <- function (inputs)
{
    n <- nrow(inputs$data)
    num.regimes <- dim(inputs$regimes)[3]
    nodes <- inputs$all.nodes
    g <- cum.g <- cum.g.unbounded <- prob.A.is.1 <- array(NaN,
                                                          dim = c(n, length(nodes$AC), num.regimes))
    if (inputs$variance.method == "ic") {
        cum.g.meanL <- cum.g.meanL.unbounded <- NULL
    }
    else {
        g.meanL <- cum.g.meanL <- cum.g.meanL.unbounded <- array(NaN,
                                                                 dim = c(n, length(nodes$AC), num.regimes, length(nodes$LY) -
                                                                                                           1))
    }
    fit <- vector("list", length(nodes$AC))
    names(fit) <- names(inputs$data)[nodes$AC]
    if (inputs$variance.method != "ic" && anyNA(inputs$regimes)) {
        regimes.meanL <- inputs$regimes
        for (i in seq_along(nodes$A)) {
            for (regime.index in 1:num.regimes) {
                regimes.meanL[is.na(regimes.meanL[, i, regime.index]),
                              i, regime.index] <- Mode(inputs$regimes[, i,
                                                                      regime.index], na.rm = TRUE)
            }
        }
    }
    else {
        regimes.meanL <- NULL
    }
    for (i in seq_along(nodes$AC)) {
        cur.node <- nodes$AC[i]
        uncensored <- IsUncensored(inputs$uncensored, nodes$C,
                                   cur.node)
        deterministic.origdata <- IsDeterministic(inputs$data,
                                                  cur.node, inputs$deterministic.Q.function, nodes,
                                                  called.from.estimate.g = TRUE, inputs$survivalOutcome)$is.deterministic
        if (is.numeric(inputs$gform)) {
            if (!is.null(inputs$deterministic.g.function))
                stop("deterministic.g.function is not compatible with numeric gform")
            prob.A.is.1[, i, ] <- inputs$gform[, i, ]
            g.est <- list(is.deterministic = deterministic.origdata)
            fit[[i]] <- "no fit due to numeric gform"
        }
        else {
            form <- inputs$gform[i]
            deterministic.g.list.origdata <- IsDeterministicG(inputs$data,
                                                              cur.node, inputs$deterministic.g.function, nodes,
                                                              using.newdata = FALSE)
            deterministic.g.origdata <- deterministic.g.list.origdata$is.deterministic
            #
            # subsetting data at the current node
            #
            if (inputs$stratify) {
                intervention.match <- InterventionMatch(inputs$intervention.match,
                  nodes$A, cur.node = nodes$AC[i])
                subs <- uncensored & intervention.match & !deterministic.origdata &
                  !deterministic.g.origdata
            }
            else {
                subs <- uncensored & !deterministic.origdata &
                  !deterministic.g.origdata
            }
            if (inputs$verbose){ message("EstimateG: estimating formula ",form," ...")}
            g.est <- Estimate(inputs, form = form, Qstar.kplus1 = NULL,
                subs = subs, family = quasibinomial(), type = "response",
                nodes = nodes, called.from.estimate.g = TRUE,
                calc.meanL = inputs$variance.method != "ic",
                cur.node = cur.node, regimes.meanL = regimes.meanL,
                regimes.with.positive.weight = 1:num.regimes)
            prob.A.is.1[, i, ] <- g.est$predicted.values
            fit[[i]] <- g.est$fit
        }
        if (cur.node %in% nodes$A) {
            cur.abar <- AsMatrix(inputs$regimes[, nodes$A ==
                cur.node, ])
            if (is.null(regimes.meanL)) {
                cur.abar.meanL <- cur.abar
            }
            else {
                cur.abar.meanL <- AsMatrix(regimes.meanL[, nodes$A ==
                  cur.node, ])
            }
        }
        else {
            cur.abar <- cur.abar.meanL <- matrix(1, nrow(inputs$data),
                num.regimes)
        }
        g[, i, ] <- CalcG(AsMatrix(prob.A.is.1[, i, ]), cur.abar,
            g.est$is.deterministic)
        if (inputs$variance.method != "ic") {
            if (is.numeric(inputs$gform)) {
                if (anyNA(g[, i, ]))
                  stop("Error - NA in numeric gform. There may not be NA values in gform (including after censoring if variance.method is 'tmle' or 'iptw'.")
                g.meanL[, i, , ] <- g[, i, ]
            }
            else {
                for (j in sseq(1, dim(g.meanL)[4])) {
                  g.meanL[, i, , j] <- CalcG(AsMatrix(g.est$prob.A.is.1.meanL[,
                    , j]), cur.abar.meanL, g.est$is.deterministic)
                }
            }
        }
        if (anyNA(g[uncensored, i, ]))
            stop("Error - NA in g. g should only be NA after censoring. If you passed numeric gform, make sure there are no NA values except after censoring. Otherwise something has gone wrong.")
    }
    for (regime.index in 1:num.regimes) {
        cum.g.list <- CalcCumG(AsMatrix(g[, , regime.index]),
            inputs$gbounds)
        cum.g[, , regime.index] <- cum.g.list$bounded
        cum.g.unbounded[, , regime.index] <- cum.g.list$unbounded
        if (inputs$variance.method != "ic") {
            for (j in sseq(1, dim(g.meanL)[4])) {
                cum.g.list <- CalcCumG(AsMatrix(g.meanL[, , regime.index,
                  j]), inputs$gbounds)
                cum.g.meanL[, , regime.index, j] <- cum.g.list$bounded
                cum.g.meanL.unbounded[, , regime.index, j] <- cum.g.list$unbounded
            }
        }
    }
    return(list(cum.g = cum.g, cum.g.unbounded = cum.g.unbounded,
        cum.g.meanL = cum.g.meanL, fit = ReorderFits(fit), prob.A.is.1 = prob.A.is.1,
        cum.g.meanL.unbounded = cum.g.meanL.unbounded))
}
