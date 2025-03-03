compareLtmle <- function(x,reference,outcome,subset,...){
    R=do.call(rbind,lapply(names(x),function(r){
        cbind(Regimen=r,summary.Ltmle(x[[r]]$Ltmle_fit))
    }))
    R[,time_horizon:=time_horizon/2]
    r1=x[[reference]]$Ltmle_fit
    time_horizon <- x[[reference]]$time_horizon
    r1.est=r1$estimate
    r1.IC=r1$IC
    D=do.call(rbind,lapply(names(x)[-match(reference,names(x))],function(reg){
        r=x[[reg]]$Ltmle_fit
        r.est=r$estimate
        r.IC=r$IC
        n <- NROW(r.IC)
        d.est=r.est-r1.est
        d.IC=r.IC-r1.IC
        d.se=apply(d.IC,2,sd)/sqrt(n)
        d.lower=d.est - qnorm(.975)*d.se
        d.upper=d.est + qnorm(.975)*d.se
        # risk ratio
        rr <- exp(log(r.est)-log(r1.est))
        log.rr.se <- apply(r.IC/r.est-r1.IC/r1.est,2,sd)/sqrt(n)
        rr.lower <- rr*exp(-qnorm(0.975)*log.rr.se)
        rr.upper <- rr*exp(qnorm(0.975)*log.rr.se)
        # cheap bootstrap
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
        boot_se=cheap_lower=cheap_upper=cheap_pvalue=vector(length(time_horizon),mode="numeric")
        rr_boot_se=rr_cheap_lower=rr_cheap_upper=rr_cheap_pvalue=vector(length(time_horizon),mode="numeric")
        if (length(r$bootfit)>0){
            for(h in seq_along(time_horizon)){
                H=time_horizon[[h]]
                cheap_ci_ate <- cheap(est=d.est[[h]],
                                      boot_est=r$bootfit[time_horizon==H]$estimate-r1$bootfit[time_horizon==H]$estimate,
                                      M=r$bootstrap_sample_size,
                                      N=r$sample_size)
                cheap_ci_rr <- cheap(est=log(rr[[h]]),
                                     boot_est=log(r$bootfit[time_horizon==H]$estimate)-log(r1$bootfit[time_horizon==H]$estimate),
                                     M=r$bootstrap_sample_size,
                                     N=r$sample_size)
                boot_se[[h]]=cheap_ci_ate$se
                cheap_lower[[h]]=cheap_ci_ate$lower
                cheap_upper[[h]]=cheap_ci_ate$upper
                cheap_pvalue[[h]]=cheap_ci_ate$pvalue
                rr_boot_se[[h]]=cheap_ci_rr$se
                rr_cheap_lower[[h]]=exp(cheap_ci_rr$lower)
                rr_cheap_upper[[h]]=exp(cheap_ci_rr$upper)
                rr_cheap_pvalue[[h]]=cheap_ci_rr$pvalue
            }
        }
        out=data.table(
            time_horizon=time_horizon/2,
            Target_parameter=c(rep("ATE",length(time_horizon)),rep("RR",length(time_horizon))),
            Regimen=rep(reg,2*length(time_horizon)),
            reference=rep(reference,2*length(time_horizon)),
            estimate=c(d.est,rr),
            std.err=c(d.se,log.rr.se),
            lower=c(d.lower,rr.lower),
            upper=c(d.upper,rr.upper),
            pvalue=c(2*(1-pnorm(abs(d.est/d.se))),2*(1-pnorm(abs(log(rr)/log.rr.se)))))
        if (length(r$bootfit)>0){
            out[,boot_se:=c(boot_se,rr_boot_se)]
            out[,cheap_lower:=c(cheap_lower,rr_cheap_lower)]
            out[,cheap_upper:=c(cheap_upper,rr_cheap_upper)]
            out[,cheap_pvalue:=c(cheap_pvalue,rr_cheap_pvalue)]
        }
        out
    }))
    R[,reference:=rep("",.N)]
    res <- rbind(R,D,use.names=TRUE)
    res[,sample_size:=r1$sample_size]
    res[,outcome:=outcome]
    res[,subset:=subset]
    if (length(r1$bootstrap_sample_size)>0)
        res[,bootstrap_sample_size:=r1$bootstrap_sample_size]
    setcolorder(res,c("sample_size","time_horizon","Target_parameter","Regimen","reference","estimate","std.err","lower","upper","pvalue"))
    res[]
}


# rr <- exp(log(r.est)-log(r1.est))
# log.rr.se <- apply(r.IC/r.est-r1.IC/r1.est,2,sd)/sqrt(n)
# rr.lower <- rr*exp(-qnorm(0.975)*log.rr.se)
# rr.upper <- rr*exp(qnorm(0.975)*log.rr.se))

