### summary.Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 19 2023 (16:43) 
## Version: 
## Last-Updated: May 19 2023 (19:27) 
##           By: Thomas Alexander Gerds
##     Update #: 7
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
summary.Ltmle <- function(object,estimator = ifelse(object$gcomp, "gcomp", "tmle"),...){
    if (length(object$estimates)>0){
        x = summary.ltmle(object,estimator = estimator)
        risk = with(x$treatment,data.table(Target_parameter = "Risk",
                                           Estimator = x$estimator,
                                           estimate = estimate,
                                           std.err = std.dev,
                                           lower = CI[[1]],
                                           upper = CI[[2]],
                                           pvalue = pvalue))
        risk
    }else{
        x = summary.ltmleEffectMeasures(object,estimator = estimator)
        ate = with(x$effect.measures$ATE,data.table(Target_parameter = "ATE",
                                                    Estimator = x$estimator,
                                                    estimate = estimate,
                                                    std.err = std.dev,
                                                    lower = CI[[1]],
                                                    upper = CI[[2]],
                                                    pvalue = pvalue))
        rr = with(x$effect.measures$RR,data.table(Target_parameter = "RelativeRisk",
                                                  Estimator = x$estimator,
                                                  estimate = estimate,
                                                  std.err = std.dev,
                                                  lower = CI[[1]],
                                                  upper = CI[[2]],
                                                  pvalue = pvalue))
        out = rbind(ate,rr)
        out
    }
}


######################################################################
### summary.Ltmle.R ends here
