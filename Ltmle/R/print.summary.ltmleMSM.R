print.summary.ltmleMSM <-
function (x, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"), 
    ...) 
{
    cat("Estimator: ", x$estimator, "\n")
    if (x$estimator == "gcomp") {
        cat("Warning: inference for gcomp is not accurate! It is based on TMLE influence curves.\n")
    }
    printCoefmat(x$cmat, digits = digits, signif.stars = signif.stars, 
        na.print = "NA", has.Pvalue = TRUE, ...)
    if (x$transformOutcome) {
        Yrange <- attr(x$transformOutcome, "Yrange")
        cat("NOTE: The MSM is modeling the transformed outcome ( Y -", 
            min(Yrange), ")/(", max(Yrange), "-", min(Yrange), 
            ")")
    }
    CheckVarianceEstimateRatio(x)
    invisible(x)
}
