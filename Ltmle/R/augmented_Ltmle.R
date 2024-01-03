Ltmle <- function(data, Anodes, Cnodes = NULL, Dnodes = NULL, Lnodes = NULL, Ynodes,
                  survivalOutcome = NULL, Qform = NULL, gform = NULL, abar, time_horizon,
                  rule = NULL, gbounds = c(0.01, 1), Yrange = NULL, deterministic.g.function = NULL,
                  deterministic.Q.function = NULL,
                  stratify = FALSE, SL.library = "glm", SL.cvControl = list(),
                  estimate.time = TRUE, gcomp = FALSE, iptw.only = FALSE, 
                  variance.method = "tmle", B = 20, m = 0.632, observation.weights = NULL, id = NULL,info = NULL,verbose=FALSE,subsampling=FALSE,...){#ltmle_dir = NULL,...){
  # if (!is.null(ltmle_dir)){
  #   for (f in list.files(paste0(ltmle_dir,"/R"),pattern = "R$",full.names = TRUE)){source(f)}
  # }
  # else if (!grepl("\\/Ltmle$",getwd())){
  #   for (f in list.files("R/",pattern = "R$",full.names = TRUE)){source(f)}
  # }
  # else{
  #   stop("please set working directory to Ltmle folder
  #        or specify the path to the Ltmle folder in ltmle_dir. ") 
  # }
  if ("glmnet" %in% SL.library)
    if (length(SL.cvControl)==0)
      SL.cvControl=list(selector="undersmooth",alpha=0.5)
  require(matrixStats)
  ## for (f in list.files("./R/",pattern = ".R$",full.names = TRUE)) {
  ## source(f)
  ## }
  ## for (f in list.files("./augmentation",pattern = ".R$",full.names = TRUE)) {
  ## source(f)
  ## }
  name_comp.event = unique(unlist(lapply(Dnodes, function(x){gsub("_[^_]*$", "", x)})))
  # name_comp.event = sub("_1","",Dnodes[[1]])
  if(length(Dnodes)>0){
    survivalOutcome=TRUE
    if (length(deterministic.Q.function)>0){
      stop("Cannot both specify deterministic.Q.function and Dnodes.")
    }
    deterministic.Q.function <- function(data, current.node, nodes, called.from.estimate.g){
      death.index <- grep(paste0(name_comp.event, "_"),names(data))
      if(length(death.index)==0)stop("No death/terminal event node found")
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
  result <- foreach(time = time_horizon, .export = "ShowGlmMessage")%do%{
    cut <- cut_Ltmle(data = data, Anodes = Anodes, Cnodes = Cnodes, Dnodes = Dnodes, Lnodes = Lnodes, Ynodes = Ynodes,
                     survivalOutcome = survivalOutcome, Qform = Qform, gform = gform, abar = abar, time_horizon = time,
                     rule = rule, gbounds = gbounds, Yrange = Yrange, deterministic.g.function = deterministic.g.function,
                     stratify = stratify, SL.library = SL.library, SL.cvControl = SL.cvControl, estimate.time = estimate.time, 
                     gcomp = gcomp, iptw.only = iptw.only, variance.method = variance.method, 
                     observation.weights = observation.weights, id = id, info = info, verbose = verbose,...)
    do.call(Ltmle_working_horse, c(cut, list(deterministic.Q.function = deterministic.Q.function))) 
  }
  if(length(time_horizon)==1){result <- result[[1]]}
  else{names(result) <- paste0("Time horizon ", time_horizon)}
  if (subsampling){
    if (m <= 0 | m >= 1) stop("m must be between 0 and 1 for subsampling")
    res_boot <- foreach(b = 1:B) %do% {
      if (verbose) message("Bootstrap ", b, " of ", B, "...")
      temp_boot <- NULL
      while (inherits(temp_boot,"simpleError") || is.null(temp_boot)){
        temp_boot <- tryCatch({foreach(time = time_horizon)%do%{
          dat_subsample <- data[sample(1:nrow(data), size = floor(m*nrow(data)), replace = FALSE),]
          cut <- cut_Ltmle(data = dat_subsample, Anodes = Anodes, Cnodes = Cnodes, Dnodes = Dnodes, Lnodes = Lnodes, Ynodes = Ynodes,
                           survivalOutcome = survivalOutcome, Qform = Qform, gform = gform, abar = abar, time_horizon = time,
                           rule = rule, gbounds = gbounds, Yrange = Yrange, deterministic.g.function = deterministic.g.function,
                           stratify = stratify, SL.library = SL.library, SL.cvControl = SL.cvControl, estimate.time = estimate.time, 
                           gcomp = gcomp, iptw.only = iptw.only, variance.method = variance.method, 
                           observation.weights = observation.weights, id = id, info = info, verbose = FALSE,...)
          do.call(Ltmle_working_horse, c(cut, list(deterministic.Q.function = deterministic.Q.function))) 
        }}, error = function(e) e)
        if (inherits(temp_boot,"simpleError")){
          message("Error in bootstrap ", b, " of ", B, ": ", temp_boot$message)
          message("Trying a new subsample")
          message("If you see a lot of these messages, consider increasing m")
        }
      }
      if(length(time_horizon)==1){temp_boot <- temp_boot[[1]]}
      else{names(temp_boot) <- paste0("Time horizon ", time_horizon)}
      temp_boot
    }
    result <- list(result = result, res_boot = res_boot, m = round(m*nrow(data)), n = nrow(data))
    class(result) <- "Ltmle_boot"
  }
  return(result)
}
