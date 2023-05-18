summarize_analysis <- function(x,outcome,time=1:5,outcome_list,raw=FALSE,merge=FALSE){
    x=copy(x)
    if (length(time)>0){
        if (match(outcome,names(x),nomatch=0))
            x=rbindlist(x[[outcome]][time])
        else # for subgroup analyses
            x=rbindlist(x[time])
    } else{
        x=x[[outcome]]
    }
    x[what!="rr",estimate:=100*estimate]
    x[what!="rr",lower:=100*lower]
    x[what!="rr",upper:=100*upper]
    x[,treatment:=factor(treatment,
                         levels=c("glp1_and_other","glp1_and_sglt2","other_and_other","sglt2_inhib_and_other"),
                         labels=c("GLP1-RA and DPP4/SU/TZD","GLP1-RA and SGLT2i","Dual DPP4/SU/TZD","SGLT2i and DPP4/SU/TZD"))]
    ## x[,reference:=factor(reference,levels=c("","glp1_and_sglt2"),labels=c("","GLP1-RA and SGLT2i"))]
    x[,reference:=factor(reference,levels=c("","other_and_other"),labels=c("","Dual DPP4/SU/TZD"))]
    if (raw){
        if ("subset"%in%names(x)){
            R=x[reference==""][,.("Subset"=subset,"Years"=horizon,"Regimen"=treatment,N=N,"estimate"=estimate,lower=lower,upper=upper)]
            D=x[what=="ate"&reference!=""][,.("Subset"=subset,"Years"=horizon,"Regimen"=treatment,N=N,"Reference"=reference,"estimate"=estimate,lower=lower,upper=upper,"p-value"=format.pval(2*pnorm(abs(estimate/100)/se,lower.tail=FALSE),eps=0.001,digits=2))]
            RR=x[what=="rr"&reference!=""][,.("Subset"=subset,"Years"=horizon,"Regimen"=treatment,N=N,"Reference"=reference,"estimate"=estimate,lower=lower,upper=upper,"p-value"="")]
        }else{
            R=x[reference==""][,.("Years"=horizon,"Regimen"=treatment,N=N,"estimate"=estimate,lower=lower,upper=upper)]
            D=x[what=="ate"&reference!=""][,.("Years"=horizon,"Regimen"=treatment,N=N,"Reference"=reference,"estimate"=estimate,lower=lower,upper=upper,"p-value"=format.pval(2*pnorm(abs(estimate/100)/se,lower.tail=FALSE),eps=0.001,digits=2))]
            RR=x[what=="rr"&reference!=""][,.("Years"=horizon,"Regimen"=treatment,N=N,"Reference"=reference,"estimate"=estimate,lower=lower,upper=upper,"p-value"="")]
        }
    }else{
        if ("subset"%in%names(x)){
            R=x[reference==""][,.("Subset"=subset,"Years"=horizon,"Regimen"=treatment,N=N,"Risk"=formatCI(x=estimate,lower=lower,upper=upper,digits=1,show.x=TRUE))]
            D=x[what=="ate"&(is.na(reference)|reference!="")][,.("Subset"=subset,"Years"=horizon,"Regimen"=treatment,N=N,"Reference"=reference,"Difference"=formatCI(x=estimate,lower=lower,upper=upper,show.x=TRUE,digits=2),"p-value"=format.pval(2*pnorm(abs(estimate/100)/se,lower.tail=FALSE),eps=0.001,digits=2))]
            RR=x[what=="rr"&(is.na(reference)|reference!="")][,.("Subset"=subset,"Years"=horizon,"Regimen"=treatment,N=N,"Reference"=reference,"Ratio"=formatCI(x=estimate,lower=lower,upper=upper,show.x=TRUE,digits=2),"p-value"="")]
        }else{
            R=x[reference==""][,.("Years"=horizon,"Regimen"=treatment,N=N,"Risk"=formatCI(x=estimate,lower=lower,upper=upper,digits=1,show.x=TRUE))]
            D=x[what=="ate"&(is.na(reference)|reference!="")][,.("Years"=horizon,"Regimen"=treatment,N=N,"Reference"=reference,"Difference"=formatCI(x=estimate,lower=lower,upper=upper,show.x=TRUE,digits=2),
                                              "p-value"=format.pval(2*pnorm(abs(estimate/100)/se,lower.tail=FALSE),eps=0.001,digits=2))]
            RR=x[what=="rr"&(is.na(reference)|reference!="")][,.("Years"=horizon,"Regimen"=treatment,N=N,"Reference"=reference,"Ratio"=formatCI(x=estimate,lower=lower,upper=upper,show.x=TRUE,digits=2),"p-value"="")]

        }
    }
    if (match(outcome,names(outcome_list),nomatch=0)){
        out <- list(R=cbind(Outcome=outcome_list[[outcome]],R),
                    D=cbind(Outcome=outcome_list[[outcome]],D),
                    RR=cbind(Outcome=outcome_list[[outcome]],RR))
    } else{
        out <- list(R=R,D=D,RR=RR)
    }
    if (merge){
        out <- merge(R,D,all.y=TRUE,all.x=TRUE,on=intersect(names(R),names(D)))
        out[,Regimen:=factor(Regimen,levels=c("GLP1-RA and SGLT2i","GLP1-RA and DPP4/SU/TZD","SGLT2i and DPP4/SU/TZD","Dual DPP4/SU/TZD"))]
        y <- out[["Years"]][1]
        out[,Years:=NULL]
        out[is.na(Reference),Reference:=""]
        out[is.na(Difference)][["p-value"]] <- ""
        out[is.na(Difference),Difference:="Reference"]
        out[,Reference:=NULL]
        setnames(out,"Risk",paste0(y,"-year  (%)risk"))
        setnames(out,"Difference",paste0(y,"-year risk difference (%)"))
        if ("Subset"%in%names(out)){
            setkey(out,Subset,Regimen)
            setkey(RR,Subset,Regimen)
            corder <- names(out)
            out <- RR[,.(Subset,Regimen,"Ratio"=Ratio)][out]
        }else{
            setkey(out,Regimen)
            setkey(RR,Regimen)
            corder <- names(out)
            out <- RR[,.(Regimen,"Ratio"=Ratio)][out]
        }
        setcolorder(out,c(corder,"Ratio"))
        out[Regimen=="Dual DPP4/SU/TZD",Ratio:="Reference"]
        setnames(out,"Ratio",paste0(y,"-year risk ratio"))
    }
    out[]
}
