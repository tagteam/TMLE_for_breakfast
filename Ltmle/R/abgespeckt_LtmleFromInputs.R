abgespeckt_LtmleFromInputs <- function (inputs) {
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
    r <- list()
    tmle <- plogis(msm.result$beta)
    r$estimates <- c("tmle" = as.numeric(tmle))
    # influence function
    r$IC <- do.call("cbind",lapply(1:length(msm.result$IC),function(j){
        msm.result$IC[[j]][, 1]* tmle[[j]] * (1 - tmle[[j]]) 
    }))
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
