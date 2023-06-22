summary.Ltmle <- function(object,estimator,...){
    if (missing(estimator))
        if (object$gcomp) estimator = "gcomp" else estimator = "tmle"
    summary_ltmle <- function (object, estimator = ifelse(object$gcomp, "gcomp", "tmle"),
              ...)
    {
        IC.variance <- var(object$IC[[estimator]])
        if (estimator == "tmle" && !is.null(object$variance.estimate)) {
            v <- max(IC.variance, object$variance.estimate)
        }
        else {
            v <- IC.variance
        }
        variance.estimate.ratio = v/IC.variance
        if (object$binaryOutcome) {
            CIBounds <- c(0, 1)
        }
        else {
            CIBounds <- c(-Inf, Inf)
        }
        treatment <- GetSummary(list(long.name = NULL, est = object$estimates[estimator],
                                     gradient = 1, log.std.err = FALSE, CIBounds = CIBounds),
                                v,
                                n = length(object$IC[[estimator]]))
        ans <- list(treatment = treatment, call = object$call, estimator = estimator,
                    variance.estimate.ratio = variance.estimate.ratio)
        class(ans) <- "summary.ltmle"
        return(ans)
    }
    summi <- function(x,target){
        with(x,data.table::data.table(
                               Target_parameter=target,
                               Estimator=estimator,
                               estimate=estimate,
                               std.err=std.dev,
                               lower=CI[[1]],
                               upper=CI[[2]],
                               pvalue=pvalue))
    }
    if (length(object$estimates)>0){
        x=summary_ltmle(object,estimator=estimator)
        risk = summi(x=x$treatment,"Risk(A=1)")
        risk
    }else{
        x= summary.ltmleEffectMeasures(object,estimator=estimator)
        treatment = summi(x=x$effect.measures$treatment,"Risk(A=1)")
        control = summi(x=x$effect.measures$control,"Risk(A=0)")
        ate = summi(x=x$effect.measures$ATE,"ATE")
        RR = summi(x=x$effect.measures$RR,"RelativeRisk")
        out=rbind(treatment,control,ate,RR)
        out
    }
}
