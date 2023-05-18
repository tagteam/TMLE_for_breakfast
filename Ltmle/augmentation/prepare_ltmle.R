prepare_ltmle <- function(primary_treatment_regimens,
                          primary_outcomes,
                          outcome,
                          regimen,
                          baseline_covariates,
                          time_covariates,
                          subset_id=NULL,
                          deterministic.Q.function,
                          abar=list(rep(1,k),rep(0,k)),
                          SL.library,
                          test=FALSE,
                          k){
    setkey(primary_treatment_regimens[[regimen]],pnr)
    primary_outcomes <- primary_outcomes[[outcome]]
    setkey(primary_outcomes,pnr)
    regimen_name <- strsplit(names(primary_treatment_regimens[[regimen]])[2],"_")[[1]][[1]]
    K <- as.numeric(strsplit(names(primary_treatment_regimens[[regimen]])[length(names(primary_treatment_regimens[[regimen]]))],"_")[[1]][[2]])
    ## message(paste0("Regimen: ",regimen_name,", number of intervals: ",K,ifelse(k<K,paste0(" analyse only ",k," intervals."),".")))
    wide_data=primary_outcomes[primary_treatment_regimens[[regimen]]]
    #
    # deal with outcome/death/censored at index
    #
    wide_data=wide_data[!(Dead_0%in%1)&!(Censored_0%in%1)]
    #
    # adding the baseline covariates
    #
    wide_data=baseline_covariates[wide_data]
    # subset and sort data
    if (test)
        work_data <- wide_data[agegroups%in%c("60-65","65-70")]
    else
        work_data <- wide_data
    # add time covariates
    # first remove outcome if overlap
    if (length((outcome_overlap <- grep(paste0(outcome,"_"),names(time_covariates)))
               >0))
        time_covariates <- time_covariates[,-outcome_overlap,with=FALSE]
    setkey(time_covariates,pnr)
    work_data=time_covariates[work_data]
    #
    # set the order of variables
    #
    names_time_covariates <- unlist(lapply(grep("_0",names(time_covariates),value=TRUE),function(x){substring(x,0,nchar(x)-2)}))
    names_baseline_covariates=setdiff(names(baseline_covariates),"pnr")
    work_data = work_data[,c("pnr",names_baseline_covariates,
                             unlist(sapply(0:k,function(timepoint){
                                 if (timepoint==0)
                                     paste0(c(names_time_covariates,regimen_name),"_",timepoint)
                                 else
                                     if (timepoint<k)
                                         paste0(c("Censored",outcome,names_time_covariates,"Dead",regimen_name),"_",timepoint)
                                 else
                                     paste0(c("Censored",outcome),"_",timepoint)
                             }))),with=FALSE]
    #
    # subset data
    #
    if (length(subset_id)>0){
        work_data <- work_data[pnr%in%subset_id]
    }
    #
    # manipulation of the event nodes
    #
    Y_nodes <- paste0(outcome,"_",1:k)
    D_nodes <- paste0("Dead","_",1:k)
    C_nodes <- paste0("Censored","_",1:k)
    Y_nodes_position <- match(Y_nodes,names(work_data))
    C_nodes_position <- match(C_nodes,names(work_data))
    D_nodes_position <- match(D_nodes,names(work_data))
    #
    # IMPORTANT: when outcome or death occurs in an interval followed by censoring,
    # we choose to uncensor the outcome and the death.
    #
    for (q in 1:k){
        if (q<k){
            has_outcome_or_death_and_censored <- (((work_data[[Y_nodes_position[[q]]]]%in%1)|(work_data[[D_nodes_position[[q]]]]%in%1)) &(work_data[[C_nodes_position[[q]]]]%in%"censored"))
        } else{
            has_outcome_or_death_and_censored <- ((work_data[[Y_nodes_position[[q]]]]%in%1)&(work_data[[C_nodes_position[[q]]]]%in%"censored"))
        }
        if (any(has_outcome_or_death_and_censored)){
            set(work_data,j=C_nodes_position[[q]],i=which(has_outcome_or_death_and_censored),value="uncensored")
        }
    }
    #
    # all nodes (but not outcome and competing risk nodes) should be NA after outcome
    # or competing risk (i.e., death) has occurred
    #
    for (Ok in Y_nodes[-k]){
        later_nodes=setdiff((match(Ok,names(work_data))+1):NCOL(work_data),Y_nodes_position)
        if (any(has_outcome <- (work_data[[Ok]]%in%1))){
            for (l in later_nodes) set(work_data,j=l,i=which(has_outcome),value=NA)
        }
    }
    if (outcome!="death"){
        # the last Death node occurs after outcome in the last interval and has been removed
        for (Dk in D_nodes[-k]){
            # for some reason Y_nodes must not be missing after death!
            later_nodes=setdiff((match(Dk,names(work_data))+1):NCOL(work_data),Y_nodes_position)
            if (any(has_died <- (work_data[[Dk]]%in%1))){
                for (l in later_nodes) set(work_data,j=l,i=which(has_died),value=NA)
            }
        }
    }
    #
    # all nodes including outcome should be NA as soon as censored occurred
    #
    for (Ck in C_nodes){
        ## later_nodes=setdiff((match(Ck,names(work_data))+1):NCOL(work_data),c(Y_nodes,D_nodes))
        later_nodes=(match(Ck,names(work_data))+1):NCOL(work_data)
        if (any(has_censored <- (work_data[[Ck]]%in%"censored"))){
            for (l in later_nodes) set(work_data,j=l,i=which(has_censored),value=NA)
        }
    }
    #
    # formulae
    #
    rhs_covariates <- function(timepoint,timevars=TRUE,regimen=TRUE){
        form=paste(names_baseline_covariates,collapse="+")
        if (timevars==TRUE) form <- paste(form,"+",paste(sapply(names_time_covariates,function(ntc){paste0(ntc,"_",timepoint-1)}),collapse="+"))
        if (regimen==TRUE){
            form <- paste0(form,"+",paste0(regimen_name,"_",0:(timepoint-1),collapse="+"))
        }
        form
    }
    # using baseline information only when k=1
    # in subsequent intervals use previous value (Markov)
    Qform <- c(unlist(lapply(1:k,function(tk){
        vv <- outcome
        # despite the Details section of help(ltmle)
        # DON't need Q-formula for the L variables
        # lalala L/Y blocks
        lapply(vv,function(v){
            outcome_form=paste0("Q.kplus1 ~ ",rhs_covariates(tk))
            names(outcome_form)=paste0(v,"_",tk)
            outcome_form
        })})))
    ## print(t(t(Qform)))
    #
    # formula for treatment and censoring
    #
    # using baseline information only
    gform <- c(paste0(regimen_name,"_",0," ~ ",rhs_covariates(timepoint=0,timevars=FALSE,regimen=FALSE)),
               paste0("Censored_",1," ~ ",rhs_covariates(timepoint=0,timevars=FALSE,regimen=FALSE)),
               paste0(regimen_name,"_",1," ~ ",rhs_covariates(timepoint=1,timevars=TRUE,regimen=TRUE)))
    # using previous value (Markov assumption)
    if (k>2){
        gform <- c(gform,unlist(lapply(2:(k-1),function(tk){
            c(paste0("Censored_",tk," ~ ",rhs_covariates(timepoint=tk,timevars=TRUE,regimen=TRUE)),
              paste0(regimen_name,"_",tk," ~ ",rhs_covariates(timepoint=tk,timevars=TRUE,regimen=TRUE)))
        })))
    }
    # last interval
    gform <- c(gform, paste0("Censored_",k," ~ ",rhs_covariates(timepoint=k,timevars=TRUE,regimen=TRUE)))
    ## print(t(t(gform)))
    Lnodes <- c(names_baseline_covariates,sapply(0:k,function(l){paste0(c(names_time_covariates,"Dead"),"_",l)}))
    Lnodes <- Lnodes[match(Lnodes,names(work_data),nomatch=0)!=0]
    list(data=work_data[],
         Qform=Qform,
         gform=gform,
         estimate.time=FALSE,
         Anodes = paste0(regimen_name,"_",0:(k-1)),
         Cnodes=paste0("Censored_",1:k),
         Lnodes = Lnodes,
         Ynodes = paste0(outcome,"_",1:k),
         survivalOutcome = TRUE,
         abar = abar,
         deterministic.Q.function = deterministic.Q.function,
         SL.library=SL.library)
}
