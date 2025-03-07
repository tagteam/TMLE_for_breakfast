run_ltmle <- function(name_outcome,
                      time_horizon,
                      sub_set=NULL,
                      test=FALSE,
                      censor_others=TRUE,
                      regimen_data,
                      outcome_data,
                      baseline_data,
                      timevar_data,
                      Markov=NULL,
                      gbounds=c(0,1),
                      gcomp=FALSE,
                      SL.library="glmnet",
                      SL.cvControl=list(selector="undersmooth",alpha=0.5),
                      B_bootstrap_samples=0,
                      bootstrap_sample_size=NULL,
                      seeds,
                      verbose=FALSE,
                      reduce=TRUE){
    require(foreach,quietly=TRUE)
    require(data.table,quietly=TRUE)
    if (!inherits(regimen_data,"list")) stop("Argument regimen_data must be a (named) list")
    if (B_bootstrap_samples>0){
        if(missing(seeds))
            seeds <- sample(1:1000000,size=B_bootstrap_samples)
    }else {
        seeds <- NULL
    }
    tk=max(time_horizon)
    if (censor_others){
        # Because A_0 = 1-B_0 we remove B_0
        abar <- c(1,rep(1:0,(tk-1)))
    } else{
        abar <- rep(1,tk)
    }
    loop <- foreach(REG = names(regimen_data),.packages=c("data.table"))%do%{
        ## [,.(pnr,sex,agegroups,index_heart_failure,tertile_income,education,diabetes_duration,secondline_duration,first_2ndline)]
        bsl_covariates <- copy(baseline_data)
        setkey(bsl_covariates,pnr)
        ## add baseline adjustment to subset analysis
        ## if (length(sub_set)>0 & length(sub_set$adj)>0){
        ##     sdat=sub_set$data[,c("pnr",sub_set$adj),with=FALSE]
        ##     setkey(sdat,pnr)
        ##     bsl_covariates <- sdat[bsl_covariates]
        ## }
        if (length(sub_set)>0){
            sub_id <- sub_set$pnr
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
        if (length(timevar_data)>0){
            if ((length(Markov)>0)&& Markov!=FALSE){
                markov <- sub("_0","",grep("_0",names(timevar_data),value=TRUE))
                if (is.character(Markov)){
                    markov = intersect(markov,Markov)
                }
            } else{
                markov=""
            }
        }else {
            markov=""
        }
        if (censor_others==TRUE)
            REG_data=copy(regimen_data[[REG]])[,B_0:=NULL]
        else
            REG_data=copy(regimen_data[[REG]])
        if (verbose)cat("Prepareing Ltmle data\n")
        # check dimensions
        n_bsl=NROW(bsl_covariates)
        n_reg=NROW(REG_data)
        n_out=NROW(outcome_data[[name_outcome]])
        n_time=NROW(timevar_data)
        if (n_reg!=n_out||n_reg!=n_time ||n_reg!=n_bsl)
            stop(paste0("Different number of persons in input:\n",
                        "\nregimen: ",n_reg,
                        "\noutcome: ",n_out,
                        "\nbaseline covariates: ",n_bsl,
                        "\ntimevarying covariates: ",n_time))
        pl=prepare_Ltmle(regimen_data=REG_data,
                         outcome_data=outcome_data,
                         name_outcome=name_outcome,
                         name_regimen=regimens,
                         name_censoring = "Censored",
                         censored_label = "censored",
                         name_competing_risk = "Dead",
                         baseline_data=bsl_covariates,
                         timevar_data=timevar_data,
                         time_horizon=time_horizon,
                         subset_id=sub_id,
                         test=test,
                         gcomp=gcomp,
                         SL.library=SL.library,
                         Markov=markov,
                         abar=abar,reduce=reduce)
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
        pl$gbounds <- gbounds
        pl$gcomp <- gcomp
        if (verbose) cat("Bootstrap repetitions: ",B_bootstrap_samples,"\n")
        if (B_bootstrap_samples>0){
            library(doParallel)
            library(foreach)
            library(data.table)
            message("Bootstrapping")
            # tb=txtProgressBar(max=B_bootstrap_samples,width=20,style=3)
            bootfit <- foreach(b=1:B_bootstrap_samples,
                               .combine="rbind",
                               .export=c("run_ltmle"),
                               .packages=c("data.table"))%dopar%{
                                   nix1=lapply(list.files("z:/Workdata/703740/R-packages/abgespeckt_Ltmle/Augmentation/", full.names = TRUE),source)
                                   nix2=lapply(list.files("z:/Workdata/703740/R-packages/abgespeckt_Ltmle/R/", full.names = TRUE), source)
                                   # setTxtProgressBar(pb=tb,b)
                                   print(paste0("seed: ",seeds[[b]],": ",b))
                                   pl.b <- data.table::copy(pl)
                                   pl.b$verbose=FALSE
                                   set.seed(seeds[[b]])
                                   # re-sampling or subsampling
                                   if (bootstrap_sample_size==NROW(pl$data)){
                                       pl.b$data <- pl$data[sample(1:.N,replace=TRUE,size=.N)]
                                   } else{
                                       stopifnot(bootstrap_sample_size<NROW(pl$data))
                                       pl.b$data <- pl$data[sample(1:.N,replace=FALSE,size=bootstrap_sample_size)]
                                   }
                                   pl.b$id=pl.b$data$pnr
                                   tryfit <- try(fit.b <- do.call(Ltmle,pl.b))
                                   if (inherits(tryfit,"try-error")) return(NULL)
                                   data.table::data.table(time_horizon=time_horizon,estimate=fit.b$estimates,b=b)
                               }
        }
        if (verbose)cat(paste0("Fitting Ltmle"," ",REG),"\n")
        tryfit <- try(fit <- do.call(Ltmle,pl))
        ## if (inherits(tryfit,"try-error"))browser()
        if (reduce){
            fit$call <- NULL
            fit$cum.g <- fit$cum.g.used <- fit$cum.g.unbounded <- NULL
            ## fit$IC <- NULL
            fit$Qstar <- NULL
            fit$fit=NULL
        }
        if (B_bootstrap_samples>0){
            fit$bootstrap_sample_size <- bootstrap_sample_size
            fit$bootfit <- bootfit
        }
        x=c(list(Ltmle_fit=fit,time_horizon=time_horizon,regimen=REG),
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
    names(loop)=names(regimen_data)[1:length(loop)]
    loop
    class(loop) <- "runLtmle"
    loop
}

