summary.Ltmle <- function(object,estimator,...){
    if (missing(estimator)) if (object$gcomp) estimator = "gcomp" else estimator = "tmle"
    summi <- function(x,target){with(x,data.table::data.table(Target_parameter=target,Estimator=estimator,estimate=estimate,std.err=std.dev,lower=CI[[1]],upper=CI[[2]],pvalue=pvalue))}
    cheap <- function(est,boot_est,M,N,mu=0,alpha=0.05){
        B <- length(boot_est)
        S <- sqrt(mean((boot_est-est)^2))
        tq <- qt(p=1-alpha/2,df=B)
        T_stat <- (est - mu) / (sqrt((M) / (N - M)) * S)
        lower=est-tq*S*sqrt(M/(N-M))
        upper=est+tq*S*sqrt(M/(N-M))
        pvalue = 2 * (1 - pt(abs(T_stat), df = B))
        list(est=est,se=S,lower=lower,upper=upper,pvalue=pvalue)
    }
    if (length(object$estimates)>0){
        if (object$binaryOutcome) {CIBounds <- c(0, 1)} else { CIBounds <- c(-Inf, Inf)}
        risk <- rbindlist(lapply(seq_along(object$estimates),function(j){
            if (NCOL(object$IC)==1)
                v <- var(object$IC)
            else
                v <- var(object$IC[,j])
            gs <- GetSummary(list(long.name = NULL,est = object$estimates[[j]],gradient = 1,log.std.err = FALSE,CIBounds = CIBounds),v,n = NROW(object$IC))
            data.table(time_horizon=object$time_horizon[[j]],
                       Target_parameter="Risk(A=1)",
                       estimate=gs$estimate,
                       std.err=gs$std.dev,
                       lower=gs$CI[[1]],
                       upper=gs$CI[[2]],
                       pvalue=gs$pvalue)
        }))
        # here
        if (length(object$bootfit)>0){
            for(h in seq_along(risk$time_horizon)){
                H=risk$time_horizon[[h]]
                cheap_ci <- cheap(est=risk$estimate[[h]],
                                  boot_est=object$bootfit[time_horizon==H]$estimate,
                                  M=object$bootstrap_sample_size,
                                  N=object$sample_size)
                risk[time_horizon==H,boot_se:=cheap_ci$se]
                risk[time_horizon==H,cheap_lower:=pmax(0,cheap_ci$lower)]
                risk[time_horizon==H,cheap_upper:=pmin(1,cheap_ci$upper)]
                risk[time_horizon==H,cheap_pvalue:=cheap_ci$pvalue]
            }
        }
        risk
    }else{
        # FIXME: at this point we need to take care of the case with multiple time horizons
        object$IC <- object$IC[[1]]
        x=summary.ltmleEffectMeasures(object,estimator=estimator)
        # check if outcome is continuous (else the estimate is called risk)
        if ("estimate"%in%names(x$effect.measures$treatment)){
            name_treatment="estimate"
            name_control="estimate"
        } else{
            name_treatment="Risk(A=1)"
            name_control="Risk(A=0)"
        }
        treatment = summi(x=x$effect.measures$treatment,target=name_treatment)
        treatment$Target_parameter="Mean(A=1)"
        control = summi(x=x$effect.measures$control,target=name_control)
        control$Target_parameter="Mean(A=0)"
        ate = summi(x=x$effect.measures$ATE,"ATE")
        if ("RR"%in%names(x$effect.measures)){
            RR = summi(x=x$effect.measures$RR,"Ratio")
        }else{
            RR=NULL
        }
        out=rbind(treatment,control,ate,RR)
        out[]
    }
}
