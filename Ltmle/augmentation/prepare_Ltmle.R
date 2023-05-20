prepare_Ltmle <- function(regimen_data,
                          outcome_data, 
                          baseline_data,
                          timevar_data,
                          time_horizon, time_interval_width_months = "NA",
                          subset_id= NULL, subset_label = "No subset",
                          name_outcome,
                          name_regimen,
                          name_censoring = "Censored",
                          censored_label = "censored",
                          name_comp.event = "Dead",
                          order_YC = FALSE,
                          Markov = NULL,
                          abar = list(rep(1,time_horizon),rep(0,time_horizon)),
                          deterministic.Q.function = NULL,
                          gcomp = FALSE,
                          iptw.only = FALSE,
                          SL.library = "glm",
                          SL.cvControl = NULL,
                          gbounds = c(0,1),
                          verbose = FALSE) {
    ## Merge all data and order in correct order
    merged_data = merge_data(time_horizon = time_horizon, regimen_data = regimen_data, outcome_data = outcome_data, baseline_data = baseline_data, 
                             timevar_data = timevar_data, name_outcome = name_outcome, name_regimen = name_regimen, 
                             name_censoring = name_censoring, censored_label = censored_label, name_comp.event = name_comp.event,
                             order_YC = order_YC, test = test)
    ## Subsetting the data; This returns data in correct order according to time and without constant nodes
    subset_data = get_subset_data(work_data = merged_data$data, time_horizon = time_horizon,
                                  subset_id = subset_id, subset_label = subset_label, Markov = Markov,
                                  name_outcome = name_outcome, name_regimen = merged_data$name_regimen,
                                  name_baseline_covariates = merged_data$name_baseline_covariates, 
                                  name_time_covariates = merged_data$name_time_covariates, 
                                  name_censoring = name_censoring, name_comp.event = name_comp.event)
    ## Change data to fit into ltmle constraints; Censored should be factor with levels "uncensored" and "censored",
    ## all nodes occurring after censoring should be NA, all nodes (except outcome) occurring after an event (outcome or competing) should be NA
    ltmle_data = get_ltmle_data(work_data = subset_data$data,
                                time_horizon = time_horizon,
                                name_outcome = name_outcome,
                                name_baseline_covariates = subset_data$baseline, # Important to use covariates from subset data as
                                name_time_covariates = subset_data$timevar,      # constant variables has been removed
                                name_regimen = subset_data$regimen,
                                name_censoring = name_censoring,
                                censored_label = censored_label,
                                name_comp.event = name_comp.event,
                                order_YC = order_YC)
    formulas = get_formulas(time_horizon = time_horizon,
                            ltmle_data = ltmle_data,
                            name_outcome = subset_data$outcome,
                            name_baseline_covariates = subset_data$baseline,
                            name_time_covariates = subset_data$timevar,
                            name_regimen = subset_data$regimen,
                            name_censoring = name_censoring,
                            name_comp.event = name_comp.event,
                            Markov = subset_data$Markov,
                            constant_variables = subset_data$constant_variables)
  
    if(length(name_regimen)==2) {
        abar = list(rep(1:0,time_horizon - 1),rep(0:1,time_horizon - 1))
    }
  
  dq <- function(data, current.node, nodes, called.from.estimate.g){
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
      ## if (names(data)[[current.node]]=="GS_2") browser()
      list(is.deterministic=is.deterministic, Q.value=0)
    }
  }
  
    det.Q.function = NULL
    if(length(deterministic.Q.function)>0){
        det.Q.function = deterministic.Q.function
    }
    if(length(deterministic.Q.function)==0&&length(name_comp.event)>0){
        if(length(grep(name_comp.event, names(ltmle_data$data), value = TRUE))>0){
            det.Q.function = dq
        }
    }
    out <- list(data = ltmle_data$data[],
                Qform = formulas$Qform,
                gform = formulas$gform,
                estimate.time = FALSE,
                Anodes = ltmle_data$Anodes,
                Cnodes = ltmle_data$Cnodes,
                Lnodes = ltmle_data$Lnodes,
                Ynodes = ltmle_data$Ynodes,
                survivalOutcome = TRUE,
                abar = abar,
                deterministic.Q.function = det.Q.function,
                SL.library=SL.library,
                gcomp = gcomp,
                gbounds = gbounds,
                iptw.only = iptw.only,
                verbose = verbose,
                info = list(time_grid = seq(0,time_horizon,1),
                            outcome = name_outcome,
                            comprisk = name_comp.event,
                            regimen = name_regimen,
                            baseline = subset_data$baseline,
                            timevar = subset_data$timevar,
                            Dnodes = intersect(paste0(name_comp.event,"_",0:time_horizon),names(ltmle_data$data)),
                            subset_label = subset_data$subset_label,
                            order_YC = order_YC,
                            time_interval = paste0("The length of the intervals is ", time_interval_width_months, " months"),
                            constants = subset_data$constant_message,
                            Markov = ifelse(length(subset_data$Markov)>0, paste(c("The following time variables are assumed to have the Markov property
                                                                         (We never assume Markov property of the regimen):", 
                                                                         subset_data$Markov),
                                                                         collapse = " "),
                                            paste0("None of the variables are assumed to have the Markov property")),
                            formulas = paste("The g-formulas are based on the assumption that the regimen at baseline depends on additional baseline covariates",
                                             "whereas, in general, the regimen depends observed covariates from previous time intervals",
                                             "since we do not know which observation happens first within the same time interval"))
                )
    if (length(SL.cvControl)>0)
        out <- c(out,list(SL.cvControl = SL.cvControl))
    class(out) <- "prepare_Ltmle"
    out
}
