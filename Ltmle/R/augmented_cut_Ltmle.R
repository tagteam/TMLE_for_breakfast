cut_Ltmle <- function(data, Anodes, Cnodes = NULL, Dnodes = NULL, Lnodes = NULL, Ynodes,
                      survivalOutcome = NULL, Qform = NULL, gform = NULL, abar, time_horizon,
                      rule = NULL, gbounds = c(0.01, 1), Yrange = NULL, deterministic.g.function = NULL,
                      deterministic.Q.function=NULL,
                      stratify = FALSE, SL.library = "glm", SL.cvControl = list(),
                      estimate.time = TRUE, gcomp = FALSE, iptw.only = FALSE, 
                      variance.method = "tmle", observation.weights = NULL, id = NULL,info = NULL, reduce = TRUE, verbose = FALSE,...){
  if(length(Ynodes)<time_horizon){
    stop("Specified time horizon is ", time_horizon, " but data only contains ", length(Ynodes), " Ynodes")
  }
  if(length(Ynodes)>time_horizon){
    Anodes <- Anodes[1:max(which(grepl(time_horizon-1, Anodes)))]
    if(length(Cnodes)>0){Cnodes <- Cnodes[1:max(which(grepl(time_horizon, Cnodes)))]}
    if(length(Dnodes)>0&&time_horizon>1){Dnodes <- Dnodes[1:max(which(grepl(time_horizon-1, Dnodes)))]}
    if(length(Dnodes)>0&&time_horizon==1){Dnodes <- NULL
    message("Dnodes have been removed as time horizon is set to 1")}
    if(length(Lnodes)>0){Lnodes <- Lnodes[1:max(which(grepl(time_horizon-1, Lnodes)))]}
    Ynodes <- Ynodes[1:max(which(grepl(time_horizon, Ynodes)))]
    lastYnode <- which(colnames(data)%in%Ynodes[length(Ynodes)])
    cols_remove <- c((lastYnode+1):length(data))
    data <- data[,!cols_remove, with = FALSE]
    
    Qform <- Qform[which(names(Qform)%in%Ynodes)]
    gform <- gform[which(names(gform)%in%c(Anodes, Cnodes))]
    
    if(is.list(abar)){
      for(x in seq_along(abar)){abar[[x]] <- abar[[x]][seq_along(Anodes)]}
    } else{abar <- abar[seq_along(Anodes)]}
    
    info$time_horizon <- time_horizon
  }
  
  list(data = data, Anodes = Anodes, Cnodes = Cnodes, Dnodes = Dnodes, Lnodes = Lnodes, Ynodes = Ynodes,
       survivalOutcome = survivalOutcome, Qform = Qform, gform = gform, abar = abar,
       rule = rule, gbounds = gbounds, Yrange = Yrange, deterministic.g.function = deterministic.g.function,
       deterministic.Q.function = deterministic.Q.function,
       stratify = stratify, SL.library = SL.library, SL.cvControl = SL.cvControl, estimate.time = estimate.time, 
       gcomp = gcomp, iptw.only = iptw.only, variance.method = variance.method, 
       observation.weights = observation.weights, id = id, info = info, reduce = reduce, verbose = verbose,...)
}
