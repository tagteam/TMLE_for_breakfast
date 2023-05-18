run_ltmle_old <- function(name_outcome,
                          time_horizon,
                          subset=NULL,
                          subsamplesize=NULL,
                          test=FALSE,
                          Markov=NULL,
                          regimen_data,
                          outcome_data,
                          baseline_data,
                          timevar_data,
                          det.Q.function,
                          SL.library="glmnet",
                          SL.cvControl=list(selector="undersmooth",alpha=0.5),
                          keep.fit=FALSE,verbose=FALSE){

    require(foreach,quietly=TRUE)
    require(data.table,quietly=TRUE)
    result <- foreach(tk=time_horizon,.combine="rbind")%do%{
        loop <- foreach(REG = names(regimen_data))%do%{
            ## print(REG)
            bsl_covariates <- baseline_data[,.(pnr,sex,agegroups,index_heart_failure,tertile_income,education,diabetes_duration,secondline_duration,first_2ndline)]
            setkey(bsl_covariates,pnr)
            ## add baseline adjustment to subset analysis
            if (length(subset)>0 & length(subset$adj)>0){
                sdat=subset$data[,c("pnr",subset$adj),with=FALSE]
                setkey(sdat,pnr)
                bsl_covariates <- sdat[bsl_covariates]
            }
            if (length(subset)>0){
                sub_id <- subset$data[["pnr"]]
                if(length(sub_id)==0)stop("No data in subset defined by variable: ",subset$var)
            } else{
                sub_id <- NULL
            }
            pl_info=prepare_data_old(regimen_data=regimen_data,outcome_data=outcome_data,
                                     name_outcome=name_outcome,name_regimen=REG,baseline_data=bsl_covariates,timevar_data=timevar_data,
                                     subset_id=sub_id,time_horizon=tk,test=test,SL.library=SL.library, Markov=Markov,
                                     deterministic.Q.function=det.Q.function,abar=rep(1,tk))
            pl=pl_info[names(pl_info)!="info"]
            if (length(subsamplesize)>0){
                pl$data <- pl$data[sample(1:NROW(pl$data),size=subsamplesize*NROW(pl$data),replace=FALSE)]
            }
            if (verbose){
                cat("Run Ltmle for regimen ",
                    REG,
                    " and outcome ",
                    name_outcome,
                    "\n",
                    sep="")
                pl$verbose <- TRUE}
            # remove apparently constant variables
            if (sum(this <- sapply(pl$data,function(x)length(unique(x))==1))>0){
                constant_vars <- names(pl$data)[this]
                message("The following variables are constant and therefore removed: ",paste0(constant_vars,collapse=", "))
                pl_info=prepare_data_old(regimen_data=regimen_data,
                                         outcome_data=outcome_data,
                                         name_outcome=name_outcome,
                                         name_regimen=REG,
                                         baseline_data=bsl_covariates[,match(names(bsl_covariates),constant_vars,nomatch=0)==0,with=FALSE],
                                         timevar_data=timevar_data[,match(names(timevar_data),constant_vars,nomatch=0)==0,with=FALSE],
                                         subset_id=sub_id,
                                         time_horizon=tk,
                                         test=test,
                                         Markov=Markov,
                                         SL.library=SL.library,
                                         deterministic.Q.function=det.Q.function,
                                         abar=rep(1,tk))
                pl=pl_info[names(pl_info) != "info"]
            }
            if (length(SL.cvControl)>0)
                pl$SL.cvControl <- SL.cvControl
            tryfit <- try(fit <- do.call(Ltmle,pl))
            if (inherits(tryfit,"try-error"))browser()
            fit
        }
        names(loop)=names(regimen_data)[1:length(loop)]
        R=do.call(rbind,lapply(names(loop),function(r){
            f=loop[[r]]
            sf=summary(f)$treatment
            data.table(treatment=r,
                       estimate=sf$estimate[[1]],
                       se=sf$std.dev,
                       lower=sf$CI[[1]],
                       upper=sf$CI[[2]])
        }))
        r1=loop[[1]]
        r1.est=r1$estimate[["tmle"]]
        r1.IC=r1$IC[["tmle"]]
        D=do.call(rbind,lapply(names(loop)[-1],function(reg){
            r=loop[[reg]]
            r.est=r$estimate[["tmle"]]
            r.IC=r$IC[["tmle"]]
            n <- length(r.IC)
            d.est=r.est-r1.est
            d.IC=r.IC-r1.IC
            d.se=sd(d.IC)/sqrt(n)
            d.lower=d.est - qnorm(.975)*d.se
            d.upper=d.est + qnorm(.975)*d.se
            rr <- exp(log(r.est)-log(r1.est))
            log.rr.se <- sd(r.IC/r.est-r1.IC/r1.est)/sqrt(n)
            rr.lower <- rr*exp(-qnorm(0.975)*log.rr.se)
            rr.upper <- rr*exp(qnorm(0.975)*log.rr.se)
            data.table(
                what=c("ate","rr"),
                treatment=rep(reg,2),
                reference=rep(names(loop)[1],2),
                estimate=c(d.est,rr),
                se=c(d.se,log.rr.se),
                lower=c(d.lower,rr.lower),
                upper=c(d.upper,rr.upper))
        }))
        R[,reference:=rep("",.N)]
        R[,what:="risk"]
        setcolorder(R,c("what","treatment","reference","estimate","se","lower","upper"))
        res <- rbind(R,D,use.names=TRUE)
        res <- cbind(cbind(N=rep(NROW(pl$data),NROW(res)),
                           horizon=rep(tk/2,NROW(res)),
                           outcome=rep(name_outcome,NROW(res))),
                     res)
        if (length(subset)>0){
            if (!is.na(subset$level))
                sub=paste0(subset$var,"=",subset$level)
            else
                sub=subset$var
            set(res,j="subset",value=rep(sub,NROW(res)))
        }
        if (keep.fit){
            fitlist <- lapply(loop, function(fit){fit$call <- NULL
                fit$cum.g <- fit$cum.g.used <- fit$cum.g.unbounded <- NULL
                fit$IC <- NULL
                fit$Qstar <- NULL
                fit
            })
            names(fitlist)=names(regimen_data)[1:length(fitlist)]
            list(result=res[],fitlist=fitlist)
        }else{
            result <- res[]
        }
    }
    result
}
