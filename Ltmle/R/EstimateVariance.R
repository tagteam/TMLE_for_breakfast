EstimateVariance <-
function(inputs, nodes, combined.summary.measures, regimes.with.positive.weight,
    uncensored, alive, Qstar, Qstar.kplus1, cur.node, msm.weights,
    LYnode.index, ACnode.index, cum.g, prob.A.is.1, cum.g.meanL,
    cum.g.unbounded, cum.g.meanL.unbounded, observation.weights,
    is.last.LYnode, intervention.match)
{
    if (inputs$variance.method == "ic")
        return(NA)
    est.var.iptw <- inputs$variance.method == "iptw"
    TmleOfVariance <- function(Z, Z.meanL) {
        if (all(is.na(Z)))
            stop("all Z are NA in EstimateVariance")
        if (length(Z.meanL) == 0 || all(Z == 0 | is.na(Z))) {
            Qstar <- Scale(Z, 0, 1)
            return(list(EZd1 = mean(Z, na.rm = TRUE), Qstar = Qstar))
        }
        if (est.var.iptw) {
            index <- uncensored & intervention.match[, d1]
            g <- cum.g[index, ACnode.index, d1]
            Y <- Scale(Z, 0, 1)[index]
            iptw.estimate <- sum(Y/g)/sum(1/g)
            return(list(EZd1 = iptw.estimate * diff(range(Z,
                na.rm = TRUE)) + min(Z, na.rm = TRUE), Qstar = rep(NA,
                length(Z))))
        }
        sparsity.data <- inputs$data[, 1:cur.node]
        sparsity.data[, cur.node] <- Scale(Z, 0, 1)
        temp.nodes <- lapply(nodes, function(x) x[x <= cur.node])
        if (cur.node %in% c(temp.nodes$D, temp.nodes$L)) {
            temp.nodes$L <- setdiff(temp.nodes$L, cur.node)
            temp.nodes$D <- setdiff(temp.nodes$D, cur.node)
            temp.nodes$Y <- c(temp.nodes$Y, cur.node)
        }
        stratify <- FALSE
        Qform <- paste(GetDefaultForm(sparsity.data[, 1:cur.node],
            nodes = temp.nodes, is.Qform = TRUE, stratify = stratify,
            survivalOutcome = FALSE, showMessage = FALSE), paste0("+ sparityAdj_Z.meanL_",
            seq_along(temp.nodes$LY)))
        Qform[length(Qform)] <- "IDENTITY"
        Z.meanL <- apply(AsMatrix(Z.meanL), 2, LogitScale)
        sparsity.data <- cbind(Z.meanL, sparsity.data)
        names(sparsity.data)[sseq(1, ncol(Z.meanL))] <- paste0("sparityAdj_Z.meanL_",
            sseq(1, ncol(Z.meanL)))
        temp.nodes <- lapply(temp.nodes, function(x) x + ncol(Z.meanL))
        names(Qform) <- names(sparsity.data)[temp.nodes$LY]
        attr(sparsity.data, "called.from.estimate.variance") <- TRUE
        var.tmle <- Ltmle(sparsity.data, Anodes = temp.nodes$A,
            Cnodes = temp.nodes$C, Dnodes = temp.nodes$D, Lnodes = temp.nodes$L, Ynodes = temp.nodes$Y,
            survivalOutcome = FALSE, Qform = Qform, gform = drop3(prob.A.is.1[,
                1:ACnode.index, d1, drop = FALSE]), abar = GetABar(inputs$regimes,
                d1, temp.nodes$A), gbounds = inputs$gbounds,
            stratify = stratify, estimate.time = FALSE, deterministic.Q.function = det.q.function,
            variance.method = "ic", observation.weights = observation.weights)
        EZd1 <- var.tmle$estimates["tmle"] * diff(range(Z, na.rm = TRUE)) +
            min(Z, na.rm = TRUE)
        return(list(EZd1 = EZd1, Qstar = var.tmle$Qstar))
    }
    EqualRegimesIndex <- function(dd1, dd2) {
        if (!any(nodes$A <= cur.node))
            return(rep(TRUE, n))
        eq <- rowAlls(AsMatrix(inputs$regimes[, which(nodes$A <=
            cur.node), dd1]) == AsMatrix(inputs$regimes[, which(nodes$A <=
            cur.node), dd2]))
        eq[is.na(eq)] <- FALSE
        return(eq)
    }
    IsStaticTreatment <- function() {
        for (dd1 in regimes.with.positive.weight) {
            for (dd2 in regimes.with.positive.weight[regimes.with.positive.weight >
                dd1]) {
                if (any(EqualRegimesIndex(dd1, dd2)))
                  return(FALSE)
            }
        }
        return(TRUE)
    }
    num.regimes <- dim(inputs$regimes)[3]
    num.betas <- ncol(combined.summary.measures)
    n <- nrow(inputs$data)
    if (inputs$survivalOutcome) {
        det.q.function <- function(data, current.node, nodes,
            called.from.estimate.g) {
            if (!any(nodes$Y < current.node))
                return(NULL)
            prev.Y <- data[, nodes$Y[nodes$Y < current.node],
                drop = FALSE]
            prev.Y[is.na(prev.Y)] <- 0
            is.deterministic <- rowAnys(prev.Y == 1)
            Q.value <- data[is.deterministic, max(nodes$Y)]
            return(list(is.deterministic = is.deterministic,
                Q.value = Q.value))
        }
    }
    else {
        det.q.function <- NULL
    }
    static.treatment <- IsStaticTreatment()
    variance.estimate <- matrix(0, num.betas, num.betas)
    Sigma <- array(dim = c(n, num.regimes, num.regimes))
    if (!is.last.LYnode)
        Q.data <- inputs$data[alive, 1:cur.node, drop = FALSE]
    for (d1 in regimes.with.positive.weight) {
        if (static.treatment) {
            d2.regimes <- d1
        }
        else {
            d2.regimes <- regimes.with.positive.weight
        }
        for (d2 in d2.regimes) {
            if (is.last.LYnode) {
                Sigma[, d1, d2] <- Qstar[, d1] * (1 - Qstar[,
                  d1])
            }
            else {
                if (any(alive)) {
                  resid.sq <- (Qstar.kplus1[alive, d1] - Qstar[alive,
                    d1]) * (Qstar.kplus1[alive, d2] - Qstar[alive,
                    d2])
                  resid.sq.range <- range(resid.sq, na.rm = TRUE)
                  if (diff(resid.sq.range) > 0.0001) {
                    Q.data[, cur.node] <- (resid.sq - resid.sq.range[1])/diff(resid.sq.range)
                    names(Q.data)[cur.node] <- "Q.kplus1"
                    m <- ltmle.glm(formula = formula(inputs$Qform[LYnode.index]),
                      family = quasibinomial(), data = Q.data,
                      weights = NULL)
                    Q.newdata <- SetA(data = Q.data, regimes = inputs$regimes[alive,
                      , d1, drop = FALSE], Anodes = nodes$A, cur.node = cur.node)
                    SuppressGivenWarnings(Q.resid.sq.pred <- predict(m,
                      newdata = Q.newdata, type = "response"),
                      "prediction from a rank-deficient fit may be misleading")
                    Sigma[alive, d1, d2] <- Q.resid.sq.pred *
                      diff(resid.sq.range) + resid.sq.range[1]
                  }
                  else {
                    resid.sq.value <- min(resid.sq, na.rm = TRUE)
                    Sigma[alive, d1, d2] <- resid.sq.value
                  }
                }
                Sigma[!alive, d1, d2] <- 0
            }
        }
    }
    if (est.var.iptw)
        Z.without.sum.meas.meanL <- Z.meanL <- NA
    no.V <- length(inputs$baseline.column.names) == 0
    if ((!est.var.iptw && static.treatment) || (est.var.iptw &&
        static.treatment && no.V)) {
        for (d1 in regimes.with.positive.weight) {
            Z.without.sum.meas <- Sigma[, d1, d1]/cum.g[, ACnode.index,
                d1] * cum.g.unbounded[, ACnode.index, d1]/cum.g[,
                ACnode.index, d1] * msm.weights[, d1]^2 * observation.weights^2
            if (!est.var.iptw)
                Z.without.sum.meas.meanL <- 1/cum.g.meanL[, ACnode.index,
                  d1, ] * cum.g.meanL.unbounded[, ACnode.index,
                  d1, ]/cum.g.meanL[, ACnode.index, d1, ] * msm.weights[,
                  d1]^2 * observation.weights^2
            var.tmle <- TmleOfVariance(Z.without.sum.meas, Z.without.sum.meas.meanL)
            if (no.V) {
                variance.estimate <- variance.estimate + (combined.summary.measures[1,
                  , d1] %*% t(combined.summary.measures[1, ,
                  d1])) * var.tmle$EZd1
            }
            else {
                baseline.msm <- paste("Qstar ~", paste(inputs$baseline.column.names,
                  collapse = " + "), "+", paste0("I(", inputs$baseline.column.names,
                  "^2)", collapse = " + "))
                V.data <- data.frame(Qstar = var.tmle$Qstar,
                  inputs$data[, inputs$baseline.column.names,
                    drop = FALSE])
                m <- ltmle.glm(formula(baseline.msm), family = quasibinomial(),
                  data = V.data, weights = NULL)
                SuppressGivenWarnings(pred.Qstar <- predict(m,
                  type = "response", newdata = V.data) * diff(range(Z.without.sum.meas,
                  na.rm = TRUE)) + min(Z.without.sum.meas, na.rm = TRUE),
                  "prediction from a rank-deficient fit may be misleading")
                variance.estimate.sum <- crossprod(combined.summary.measures[,
                  , d1], combined.summary.measures[, , d1] *
                  pred.Qstar)
                variance.estimate <- variance.estimate + variance.estimate.sum/n
            }
        }
    }
    else {
        for (beta.index2 in 1:num.betas) {
            for (d1 in regimes.with.positive.weight) {
                Z.base <- rep(0, n)
                if (!est.var.iptw)
                  Z.base.meanL <- matrix(0, n, dim(cum.g.meanL)[4])
                for (d2 in regimes.with.positive.weight) {
                  equal.regimes.index <- EqualRegimesIndex(d1,
                    d2)
                  h1 <- combined.summary.measures[, beta.index2,
                    d2] * msm.weights[, d2]
                  Z.base[equal.regimes.index] <- Z.base[equal.regimes.index] +
                    h1[equal.regimes.index] * Sigma[equal.regimes.index,
                      d1, d2]/cum.g[equal.regimes.index, ACnode.index,
                      d1] * observation.weights[equal.regimes.index]
                  if (!est.var.iptw)
                    Z.base.meanL[equal.regimes.index, ] <- Z.base.meanL[equal.regimes.index,
                      ] + h1[equal.regimes.index] * 1/cum.g.meanL[equal.regimes.index,
                      ACnode.index, d1, ] * observation.weights[equal.regimes.index]
                }
                for (beta.index1 in 1:num.betas) {
                  if (beta.index1 >= beta.index2) {
                    Z <- combined.summary.measures[, beta.index1,
                      d1] * msm.weights[, d1] * cum.g.unbounded[,
                      ACnode.index, d1]/cum.g[, ACnode.index,
                      d1] * observation.weights * Z.base
                    if (!est.var.iptw)
                      Z.meanL <- combined.summary.measures[,
                        beta.index1, d1] * msm.weights[, d1] *
                        cum.g.meanL.unbounded[, ACnode.index,
                          d1, ]/cum.g.meanL[, ACnode.index, d1,
                        ] * observation.weights * Z.base.meanL
                    var.tmle <- TmleOfVariance(Z, Z.meanL)
                    variance.estimate[beta.index1, beta.index2] <- variance.estimate[beta.index1,
                      beta.index2] + var.tmle$EZd1
                  }
                  else {
                    variance.estimate[beta.index1, beta.index2] <- variance.estimate[beta.index2,
                      beta.index1]
                  }
                }
            }
        }
    }
    if (max(abs(variance.estimate - t(variance.estimate))) >
        1.0000000000000001e-05)
        stop("not symmetric")
    if (any(eigen(variance.estimate, only.values = TRUE)$values <
        -1e-08)) {
        variance.estimate <- MakePosDef(variance.estimate)
    }
    return(variance.estimate)
}
