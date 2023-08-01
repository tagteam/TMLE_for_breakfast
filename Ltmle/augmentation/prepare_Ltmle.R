prepare_Ltmle <- function(regimen_data,
                          outcome_data,
                          baseline_data,
                          timevar_data,
                          time_horizon,
                          subset_id = NULL,
                          subset_label = NULL,
                          name_outcome,
                          name_regimen,
                          name_censoring = "Censored",
                          censored_label = "censored",
                          name_comp.event = "Dead",
                          Markov = NULL,
                          abar,
                          test = FALSE,...) {
    ## Merge all data and order in correct order
    merged_data = merge_and_sort_data(time_horizon = time_horizon,
                                      regimen_data = regimen_data,
                                      outcome_data = outcome_data,
                                      baseline_data = baseline_data,
                                      timevar_data = timevar_data,
                                      name_outcome = name_outcome,
                                      name_regimen = name_regimen,
                                      name_censoring = name_censoring,
                                      censored_label = censored_label,
                                      name_comp.event = name_comp.event,
                                      test = test)

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
                                name_baseline_covariates = subset_data$name_baseline_covariates,
                                name_time_covariates=merged_data$name_time_covariates,
                                name_regimen=name_regimen,
                                name_censoring=name_censoring,
                                censored_label=censored_label,
                                name_comp.event=name_comp.event)

    formulas = get_formulas(time_horizon = time_horizon,
                            work_data = ltmle_data$data,
                            name_outcome = name_outcome,
                            name_baseline_covariates = subset_data$name_baseline_covariates,
                            name_time_covariates = merged_data$name_time_covariates,
                            name_regimen = name_regimen,
                            name_censoring = name_censoring,
                            name_comp.event = name_comp.event,
                            Markov = Markov,
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
                     regimen = name_regimen,
                     baseline = subset_data$name_baseline_covariates,
                     timevar = merged_data$name_time_covariates,
                     subset_label = subset_data$subset_label,
                     order = merged_data$order,
                     time_horizon = max(time_horizon),
                     time_interval = time_interval,
                     at.risk = ltmle_data$at.risk,
                     constant_variables= subset_data$constant_variables,
                     Markov = Markov),
         ...)
}
