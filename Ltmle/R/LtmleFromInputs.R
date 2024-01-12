LtmleFromInputs <- function (inputs)
{
    if (inputs$verbose){
        message("LtmleFromInputs: -> LtmleMSMFromInputs ...")
    }
    msm.result <- LtmleMSMFromInputs(inputs)
    if (inputs$verbose){ message("LtmleFromInputs: transforming results ...")}
    num.regimes <- dim(inputs$regimes)[3]
    stopifnot(num.regimes %in% 1:2)
    if (num.regimes == 2) {
        class(msm.result) <- "ltmleEffectMeasures"
        return(msm.result)
    }
    names(msm.result$beta.iptw) <- names(msm.result$beta) <- NULL
    if (!is.null(msm.result$beta.iptw)){
      iptw <- plogis(msm.result$beta.iptw)
      iptw.list <- list(iptw.estimate = iptw, iptw.IC = iptw *
                          (1 - iptw) * msm.result$IC.iptw[, 1])
    }
    else {
      iptw <- iptw.list <- NULL
    }
    r <- list()
    if (inputs$iptw.only) {
        tmle <- NA
        tmle.IC <- rep(NA, nrow(inputs$data))
    }
    else {
        tmle <- plogis(msm.result$beta)
        tmle.IC <- msm.result$IC[, 1]
    }
    r$estimates <- c(tmle = tmle, iptw = iptw.list$iptw.estimate)
    r$IC <- list(tmle = tmle.IC * tmle * (1 - tmle), iptw = iptw.list$iptw.IC)
    if (!is.null(msm.result$variance.estimate)) {
        stopifnot(length(msm.result$variance.estimate) == 1)
        r$variance.estimate <- msm.result$variance.estimate[1] *
            (tmle * (1 - tmle))^2
    }
    if (inputs$gcomp) {
        names(r$estimates)[1] <- names(r$IC)[1] <- "gcomp"
    }
    r$cum.g <- AsMatrix(msm.result$cum.g[, , 1])
    r$cum.g.unbounded <- AsMatrix(msm.result$cum.g.unbounded[,
        , 1])
    r$cum.g.used <- AsMatrix(msm.result$cum.g.used[, , 1])
    r$gcomp <- inputs$gcomp
    r$fit <- msm.result$fit
    r$fit$g <- r$fit$g[[1]]
    r$fit$Q <- r$fit$Q[[1]]
    r$Qstar <- msm.result$Qstar[, 1, 1]
    r$formulas <- msm.result$formulas
    r$binaryOutcome <- msm.result$binaryOutcome
    r$transformOutcome <- msm.result$transformOutcome == TRUE
    if (msm.result$transformOutcome) {
        Yrange <- attr(msm.result$transformOutcome, "Yrange")
        r$estimates <- r$estimates * diff(Yrange) + min(Yrange)
        r$IC <- lapply(r$IC, function(IC) IC * diff(Yrange))
        r$variance.estimate <- r$variance.estimate * (diff(Yrange))^2
    }
    class(r) <- "ltmle"
    return(r)
}
