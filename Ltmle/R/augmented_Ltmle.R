Ltmle <- function(data, Anodes, Cnodes = NULL, Dnodes = NULL, Lnodes = NULL, Ynodes, final.Ynodes=NULL,
                  survivalOutcome = NULL, Qform = NULL, gform = NULL, abar, time_horizon,
                  rule = NULL, gbounds = c(0.01, 1), Yrange = NULL, deterministic.g.function = NULL,
                  deterministic.Q.function = NULL,
                  stratify = FALSE, SL.library = "glm", SL.cvControl = list(),
                  estimate.time = TRUE, gcomp = FALSE, iptw.only = FALSE,
                  variance.method = "ic", observation.weights = NULL, id = NULL,info = NULL,verbose=FALSE,reduce=TRUE,...){
    if (NROW(data) == 0) stop("Argument 'data' seems empty.")
    require(matrixStats)
    if (SL.library=="glmnet" && (length(SL.cvControl)==0)) SL.cvControl=list(selector="undersmooth",alpha=0.5)
    if (length(Cnodes)>0||length(Dnodes)>0){survivalOutcome=TRUE}
    if(length(Dnodes)>0){
        name_competing_risk = gsub("_[^_]*$", "", Dnodes[[1]])
        deterministic.Q.function <- function(data, current.node, nodes, called.from.estimate.g){
            death.index <- grep(paste0("Dead", "_"),names(data))
            if(length(death.index)==0){
                message("No death/terminal event node found")
                return(NULL)
            }
            hist.death.index <- death.index[death.index < current.node]
            if(length(hist.death.index)==0)
                return(NULL)
            else{
                is.deterministic <- Reduce("+",lapply(data[,hist.death.index,drop=FALSE],
                                                      function(dd){x=dd;x[is.na(dd)] <- 0;x}))>=1
                # should be unnecessary to exclude those who readily
                # have a missing value for death, but it does not hurt either
                is.deterministic[is.na(is.deterministic)] <- FALSE
                list(is.deterministic=is.deterministic, Q.value=0)
            }
        }
    }
    # multiple time horizons
    if (length(final.Ynodes)==0)
        final.Ynodes=sapply(time_horizon,function(t){grep(paste0("_",t),Ynodes,value=TRUE)})
    max_time_horizon <- max(time_horizon)
    if(length(Ynodes)<max_time_horizon){
        stop("Specified maximal time horizon is ", max_time_horizon, " but data only contains ", length(Ynodes), " Ynodes")
    }
    # cut the crap
    if(length(Ynodes)>max_time_horizon){
        Anodes <- Anodes[1:max(which(grepl(max_time_horizon-1, Anodes)))]
        if(length(Cnodes)>0){Cnodes <- Cnodes[1:max(which(grepl(max_time_horizon, Cnodes)))]}
        if(length(Dnodes)>0&max_time_horizon>1){Dnodes <- Dnodes[1:max(which(grepl(max_time_horizon-1, Dnodes)))]}
        if(length(Dnodes)>0&max_time_horizon==1){Dnodes <- NULL
            message("Dnodes have been removed as time horizon is set to 1")}
        if(length(Lnodes)>0){Lnodes <- Lnodes[1:max(which(grepl(max_time_horizon-1, Lnodes)))]}
        Ynodes <- Ynodes[1:max(which(grepl(max_time_horizon, Ynodes)))]
        lastYnode <- which(colnames(data)%in%Ynodes[length(Ynodes)])
        cols_remove <- c((lastYnode+1):length(data))
        data <- data[,!cols_remove, with = FALSE]
        Qform <- Qform[which(names(Qform)%in%Ynodes)]
        gform <- gform[which(names(gform)%in%c(Anodes, Cnodes))]
        if(is.list(abar)){
            for(x in 1:length(abar)){abar[[x]] <- abar[[x]][1:length(Anodes)]}
        } else{abar <- abar[1:length(Anodes)]}
    }
    data <- CheckData(data)
    msm.inputs <- GetMSMInputsForLtmle(data, abar, rule, gform,final.Ynodes=final.Ynodes)
    inputs <- CreateInputs(data = data, Anodes = Anodes, Cnodes = Cnodes, Dnodes = Dnodes,
                           Lnodes = Lnodes, Ynodes = Ynodes, survivalOutcome = survivalOutcome,
                           Qform = Qform, gform = msm.inputs$gform, Yrange = Yrange,
                           gbounds = gbounds, deterministic.g.function = deterministic.g.function,
                           SL.library = SL.library, SL.cvControl = SL.cvControl,
                           regimes = msm.inputs$regimes, working.msm = msm.inputs$working.msm,
                           summary.measures = msm.inputs$summary.measures, final.Ynodes = msm.inputs$final.Ynodes,
                           stratify = stratify, msm.weights = msm.inputs$msm.weights,
                           estimate.time = estimate.time, gcomp = gcomp, iptw.only = iptw.only,
                           deterministic.Q.function = deterministic.Q.function,
                           variance.method = variance.method, observation.weights = observation.weights,
                           id = id, verbose = verbose)
    result <- abgespect_LtmleFromInputs(inputs)
    result <- c(result,match.call()$info)
    if (reduce){
        result$cum.g <- result$cum.g.used <- result$cum.g.unbounded <- NULL
        result$Qstar <- NULL
        result$fit$Qstar <- NULL
    }else{
        result$call <- match.call()
    }
    result$sample_size <- NROW(data)
    result$time_horizon <- time_horizon
    class(result) <- "Ltmle"
    return(result)
}
