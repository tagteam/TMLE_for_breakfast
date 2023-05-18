summary.ltmleEffectMeasures <-
function (object, estimator = ifelse(object$gcomp, "gcomp", "tmle"), 
    ...) 
{
    info <- GetSummaryLtmleMSMInfo(object, estimator)
    beta <- info$estimate
    IC <- info$IC
    y0 <- plogis(beta[1])
    y1 <- plogis(beta[1] + beta[2])
    names(y0) <- names(y1) <- NULL
    eff.list <- list(treatment = list(long.name = "Treatment Estimate", 
        est = y1, gradient = c(y1 * (1 - y1), y1 * (1 - y1)), 
        log.std.err = FALSE, CIBounds = 0:1), control = list(long.name = "Control Estimate", 
        est = y0, gradient = c(y0 * (1 - y0), 0), log.std.err = FALSE, 
        CIBounds = 0:1), ATE = list(long.name = "Additive Treatment Effect", 
        est = y1 - y0, gradient = c(y1 * (1 - y1) - y0 * (1 - 
            y0), y1 * (1 - y1)), log.std.err = FALSE, CIBounds = c(-1, 
            1)), RR = list(long.name = "Relative Risk", est = y1/y0, 
        gradient = c(y0 - y1, 1 - y1), log.std.err = TRUE, CIBounds = c(0, 
            Inf)), OR = list(long.name = "Odds Ratio", est = exp(beta[2]), 
        gradient = c(0, 1), log.std.err = TRUE, CIBounds = c(0, 
            Inf)))
    if (!object$binaryOutcome) {
        eff.list$RR <- eff.list$OR <- NULL
    }
    n <- nrow(IC)
    measures.IC <- lapply(eff.list, GetSummary, var(IC), n)
    if (is.null(object$variance.estimate)) {
        measures.variance.estimate <- NULL
    }
    else {
        measures.variance.estimate <- lapply(eff.list, GetSummary, 
            object$variance.estimate, n)
    }
    measures.max <- measures.IC
    for (i in seq_along(measures.variance.estimate)) {
        std.dev.diff <- measures.variance.estimate[[i]]$std.dev - 
            measures.IC[[i]]$std.dev
        if (!is.na(std.dev.diff) && (std.dev.diff > 0)) {
            measures.max[[i]] <- measures.variance.estimate[[i]]
        }
    }
    if (object$transformOutcome) {
        Yrange <- attr(object$transformOutcome, "Yrange")
        measures.max <- lapply(measures.max, function(x) {
            x$estimate <- x$estimate * diff(Yrange)
            x$std.dev <- x$std.dev * diff(Yrange)
            x$CI <- x$CI * diff(Yrange)
            if (x$long.name %in% c("Treatment Estimate", "Control Estimate")) {
                x$estimate <- x$estimate + min(Yrange)
                x$CI <- x$CI + min(Yrange)
            }
            else {
                stopifnot(x$long.name == "Additive Treatment Effect")
            }
            return(x)
        })
    }
    ans <- list(call = object$call, effect.measures = measures.max, 
        variance.estimate.ratio = info$variance.estimate.ratio, 
        estimator = estimator)
    class(ans) <- "summary.ltmleEffectMeasures"
    return(ans)
}
