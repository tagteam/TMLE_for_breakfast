run_ltmle <- function(name_outcome,
                      time_horizon,
                      sub_set=NULL,
                      censor_others=TRUE,
                      regimen_data,
                      outcome_data,
                      baseline_data,
                      timevar_data,
                      Markov=NULL,
                      SL.library="glmnet",
                      SL.cvControl=list(selector="undersmooth",alpha=0.5),
                      verbose=FALSE,
                      reduce=TRUE,
                      B_0 = FALSE,gbounds){
    result <- foreach(tk=time_horizon)%do%{
        if (censor_others){
            # Because A_0 = 1-B_0 we remove B_0
            abar <- c(1,rep(1:0,(tk-1)))
            if(B_0){
                abar <- rep(1:0,tk)
            }
        } else{
            abar <- rep(1,tk)
        }
        loop <- foreach(REG = names(regimen_data))%do%{
            ## [,.(pnr,sex,agegroups,index_heart_failure,tertile_income,education,diabetes_duration,secondline_duration,first_2ndline)]
            bsl_covariates <- copy(baseline_data)
            setkey(bsl_covariates,pnr)
            ## add baseline adjustment to subset analysis
            if (length(sub_set)>0 && length(sub_set$adj)>0){
                sdat=sub_set$data[,c("pnr",sub_set$adj),with=FALSE]
                setkey(sdat,pnr)
                bsl_covariates <- sdat[bsl_covariates]
            }
            if (length(sub_set)>0){
                sub_id <- sub_set$data[["pnr"]]
                if(length(sub_id)==0)stop("No data in subset defined by variable: ",sub_set$var)
            } else{
                sub_id <- NULL
            }
            if (censor_others){
                regimens <- c(REG,"B")
            }else{
                regimens <- REG
            }
            # all timevarying covariates but not treatment
            # should only enter the formula with their last value
            if (length(Markov)>0){
                markov <- sub("_0","",grep("_0",names(timevar_data),value=TRUE))
                if (is.character(Markov)){
                    markov = intersect(markov,Markov)
                }
            } else{
                markov=""
            }
            if (censor_others==TRUE&&!B_0){
                suppressWarnings(REG_data <- copy(regimen_data[[REG]])[,B_0:=NULL])
            }else{REG_data=copy(regimen_data[[REG]])}
            setDT(REG_data)
            setkey(REG_data,pnr)
            setkey(outcome_data,pnr)
            pl=prepare_Ltmle(regimen_data=REG_data,
                             outcome_data=outcome_data,
                             name_outcome=name_outcome,
                             name_regimen=regimens,
                             name_censoring = "Censored",
                             censored_label = "censored",
                             name_comp.event = "Dead",
                             baseline_data=bsl_covariates,
                             timevar_data=timevar_data,
                             time_horizon=tk,
                             subset_id=sub_id,
                             SL.library=SL.library,
                             Markov=markov,
                             abar=abar)
            if (verbose){
                cat("Run Ltmle for regimen ",
                    paste0(regimens,collapse=","),
                    " and outcome ",
                    name_outcome,
                    "\n",
                    sep="")
                pl$verbose <- TRUE}
            if (!missing(gbounds)){
                pl$gbounds <- gbounds
            }
            if (length(SL.cvControl)>0)
                pl$SL.cvControl <- SL.cvControl
            if (verbose)print(paste0("Fitting Ltmle"," ",REG))
            try(fit <- do.call(Ltmle,pl))
            fit$info$estimator <- "tmle"
            if (gcomp) fit$info$estimator <- "gcomp"
            if (iptw.only) fit$info$estimator <- "iptw"
            if (reduce){
                fit$call <- NULL
                fit$cum.g <- fit$cum.g.used <- fit$cum.g.unbounded <- NULL
                ## fit$IC <- NULL
                fit$Qstar <- NULL
            }
            x=c(list(Ltmle_fit=fit,time_horizon=tk,regimen=REG),
                # formula are potential data/environment collectors
                # when object is saved hence we not include them
                # in the output
                ## Qform=Qform,
                ## gform=gform,
                with(pl,list(Anodes=Anodes,
                             Cnodes=Cnodes,
                             Lnodes=Lnodes,
                             Dnodes=Dnodes,
                             Ynodes=Ynodes,
                             abar=abar,
                             SL.library=SL.library,
                             SL.cvControl=SL.cvControl)))
            x
        }
        names(loop)=names(regimen_data)[seq_along(loop)]
        loop
    }
    names(result)=paste0("time_horizon_",time_horizon)
    result
}

targetedRegisterCourse_run_Ltmle <- function(name_outcome,
                                             name_competing_risk = "Dead",
                                             name_censoring = "Censored",
                                             censored_label = "censored",
                                             time_horizon,
                                             sub_set=NULL,
                                             regimen_data,
                                             outcome_data,
                                             baseline_data,
                                             timevar_data,
                                             abar,
                                             gcomp = FALSE,
                                             iptw.only = FALSE,
                                             Markov=NULL,
                                             SL.library,
                                             SL.cvControl,
                                             verbose=FALSE,
                                             reduce=TRUE){
    require(foreach,quietly=TRUE)
    require(data.table,quietly=TRUE)
    if (missing(SL.cvControl)){
        if (GetLibrary(SL.library,"Q")[[1]] == "glmnet")
            SL.cvControl = list(selector="undersmooth",alpha=0.5)
        else
            SL.cvControl = NULL
    }
    ## loading augmentedLtmle
    ## tar_source("../Ltmle")
    result <- foreach(tk=time_horizon)%do%{
        if (missing(abar)){
            abar <- rep(1,tk)
        }
        loop <- foreach(REG = names(regimen_data))%do%{
            ## [,.(pnr,sex,agegroups,index_heart_failure,tertile_income,education,diabetes_duration,secondline_duration,first_2ndline)]
            bsl_covariates <- copy(baseline_data)
            data.table::setkey(bsl_covariates,pnr)
            ## add baseline adjustment to subset analysis
            if (length(sub_set)>0 && length(sub_set$adj)>0){
                sdat=sub_set$data[,c("pnr",sub_set$adj),with=FALSE]
                data.table::setkey(sdat,pnr)
                bsl_covariates <- sdat[bsl_covariates]
            }
            if (length(sub_set)>0){
                sub_id <- sub_set$data[["pnr"]]
                if(length(sub_id)==0)stop("No data in subset defined by variable: ",sub_set$var)
            } else{
                sub_id <- NULL
            }
            regimens <- REG
            # all timevarying covariates but not treatment
            # should only enter the formula with their last value
            if (length(Markov)>0){
                markov <- sub("_0","",grep("_0",names(timevar_data),value=TRUE))
                if (is.character(Markov)){
                    markov = intersect(markov,Markov)
                }
            } else{
                markov=""
            }
            REG_data=copy(regimen_data[[REG]])
            if (name_outcome == "Dead"){
                pl=prepare_Ltmle(regimen_data=REG_data,
                                 outcome_data=outcome_data,
                                 name_outcome=name_outcome,
                                 name_regimen=regimens,
                                 name_censoring = name_censoring,
                                 name_competing_risk = NULL,
                                 survivalOutcome = TRUE,
                                 censored_label = censored_label,
                                 baseline_data=bsl_covariates,
                                 timevar_data=timevar_data,
                                 time_horizon=tk,
                                 subset_id=sub_id,
                                 SL.library=SL.library,
                                 Markov=markov,
                                 abar=abar)
            } else
                pl=prepare_Ltmle(regimen_data=REG_data,
                                 outcome_data=outcome_data,
                                 name_outcome=name_outcome,
                                 name_regimen=regimens,
                                 name_censoring = name_censoring,
                                 censored_label = censored_label,
                                 name_competing_risk = name_competing_risk,
                                 survivalOutcome = TRUE,
                                 baseline_data=bsl_covariates,
                                 timevar_data=timevar_data,
                                 time_horizon=tk,
                                 subset_id=sub_id,
                                 SL.library=SL.library,
                                 Markov=markov,
                                 abar=abar)
            if (verbose){
                cat("Run Ltmle for regimen ",
                    paste0(regimens,collapse=","),
                    " and outcome ",
                    name_outcome,
                    "\n",
                    sep="")
                pl$verbose <- TRUE}
            if (length(SL.cvControl)>0)
                pl$SL.cvControl <- SL.cvControl
            if (verbose)print(paste0("Fitting Ltmle"," ",REG))
            if (gcomp) pl$gcomp <- gcomp
            if (iptw.only) pl$iptw.only <- iptw.only
            try(fit <- do.call(Ltmle,pl))
            fit$info$estimator <- "tmle"
            if (gcomp) fit$info$estimator <- "gcomp"
            if (iptw.only) fit$info$estimator <- "iptw"
            ## if (inherits(tryfit,"try-error"))browser()
            if (reduce){
                fit$call <- NULL
                fit$cum.g <- fit$cum.g.used <- fit$cum.g.unbounded <- NULL
                ## fit$IC <- NULL
                fit$Qstar <- NULL
            }
            x=c(list(Ltmle_fit=fit,time_horizon=tk,regimen=REG),
                # formula are potential data/environment collectors
                # when object is saved hence we not include them
                # in the output
                ## Qform=Qform,
                ## gform=gform,
                with(pl,list(Anodes=Anodes,
                             Cnodes=Cnodes,
                             Lnodes=Lnodes,
                             Dnodes=Dnodes,
                             Ynodes=Ynodes,
                             abar=abar,
                             SL.library=SL.library,
                             SL.cvControl=SL.cvControl)))
            x
        }
        names(loop)=names(regimen_data)[seq_along(loop)]
        loop
    }
    names(result)=paste0("time_horizon_",time_horizon)
    class(result) = "runLtmle"
    result
}
