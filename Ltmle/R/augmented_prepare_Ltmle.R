prepare_Ltmle <- function(regimen_data,
                          outcome_data,
                          baseline_data,
                          timevar_data,
                          time_horizon,
                          subset_id = NULL,
                          subset_label = NULL,
                          name_outcome,
                          name_regimen,
                          name_censor_others = NULL,
                          name_censoring = "Censored",
                          censored_label = "censored",
                          name_competing_risk = "Dead",
                          Markov = NULL,
                          abar,
                          independent_regimens = FALSE,
                          ...) {
    if (!inherits(outcome_data,"data.frame")) {
        stop("Argument 'outcome_data' must be a data.frame or data.table or tibble.")
    }
    stopifnot(length(grep(name_regimen,names(regimen_data)))>0)
    stopifnot(length(grep(name_outcome,names(outcome_data)))>0)
    if (length(name_competing_risk)>0)
        stopifnot(length(grep(name_competing_risk,names(outcome_data)))>0)
    if (length(name_censoring)>0){
        stopifnot(length(cnodes <- grep(name_censoring,names(outcome_data)))>0)
        if(is.na(match(censored_label,
                       unique(c(sapply(outcome_data[,cnodes,with = FALSE],function(x)unique(x)))))))
            stop("Censored label does not occur in Censoring nodes")
    }
    if (missing(abar))stop("abar is missing.")
    ## Merge all data and order in correct order
    merged_data = merge_and_sort_data(time_horizon = time_horizon,
                                      regimen_data = regimen_data,
                                      outcome_data = outcome_data,
                                      baseline_data = baseline_data,
                                      timevar_data = timevar_data,
                                      name_outcome = name_outcome,
                                      name_regimen = unlist(name_regimen),
                                      name_censoring = name_censoring,
                                      censored_label = censored_label,
                                      name_competing_risk = name_competing_risk)
  ## Subsetting the data; This returns data in correct order according to time and without constant nodes
  subset_data = get_subset_data(work_data = merged_data$data,
                                time_horizon = time_horizon,
                                subset_id = subset_id,
                                subset_label = subset_label,
                                name_baseline_covariates = merged_data$name_baseline_covariates)
  
  ## Change data to fit into ltmle constraints; Censored should be factor with levels "uncensored" and "censored",
    ## all nodes occurring after censoring should be NA, all nodes (except outcome) occurring after an event (outcome or competing) should be NA
  
  ltmle_data = get_ltmle_data(subset_data$data,
                              time_horizon = time_horizon,
                              name_outcome = name_outcome,
                              name_time_covariates = merged_data$name_time_covariates,
                              name_regimen = unlist(name_regimen),
                              name_censoring = name_censoring,
                              censored_label = censored_label,
                              name_competing_risk = name_competing_risk,
                              abar=abar)
  
  formulas = get_formulas(time_horizon = time_horizon,
                          work_data = ltmle_data$data,
                          name_outcome = name_outcome,
                          name_baseline_covariates = subset_data$name_baseline_covariates,
                          name_time_covariates = merged_data$name_time_covariates,
                          name_regimen = name_regimen,
                          name_censoring = name_censoring,
                          name_competing_risk = name_competing_risk,
                          Markov = Markov,
                          independent_regimens = independent_regimens,
                          constant_variables = subset_data$constant_variables)
  
  ## abar
  if (missing(abar)){
    if(length(name_regimen)==2) {
      abar <- list(rep(1:0,max(time_horizon)),rep(0:1,max(time_horizon)))}
    else
      abar <- list(rep(1,max(time_horizon)), rep(0,max(time_horizon)))
  }
  
  ## Message about the time interval
  time_interval = NULL
  list(data = ltmle_data$data[],
       Qform = formulas$Qform,
       gform = formulas$gform,
       estimate.time = FALSE,
       Anodes = ltmle_data$Anodes,
       Cnodes = ltmle_data$Cnodes,
       Dnodes = ltmle_data$Dnodes,
       Lnodes = ltmle_data$Lnodes,
       Ynodes = ltmle_data$Ynodes,
       abar = abar,
       time_horizon = time_horizon,
       info = list(outcome = name_outcome,
                   regimen = unlist(name_regimen),
                   abar = abar,
                   baseline = subset_data$name_baseline_covariates,
                   timevar = merged_data$name_time_covariates,
                   subset_label = subset_data$subset_label,
                   order = merged_data$order,
                   time_horizon = max(time_horizon),
                   time_interval = time_interval,
                   event_counts= ltmle_data$event_counts,
                   constant_variables= subset_data$constant_variables,
                   Markov = Markov),
       ...)
}
