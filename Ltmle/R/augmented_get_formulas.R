## Get g-formulas and Q-formulas

get_formulas <- function(time_horizon,
                         work_data,
                         name_outcome,
                         name_baseline_covariates,
                         name_time_covariates,
                         name_regimen,
                         name_censoring = NULL,
                         name_competing_risk = NULL,
                         Markov = NULL, ## Names of time varying covariates with Markov property. Note that regimen is assumed NOT to be Markov
                         constant_variables = NULL,
                         independent_regimens = FALSE){
  if (length(Markov)>0 && Markov[[1]]!="")
    if (any(not_found <- !(Markov%in%name_time_covariates)))
      stop(paste0("The following variables in argument Markov do not match time_covariates:\n",
                  paste(Markov[not_found],collapse=", ")))
  
  time_horizon = max(time_horizon)
  gform = if(independent_regimens){
    c(unlist(lapply(name_regimen, function(reg){paste0(reg,"_0"," ~ ", 
                                                       get_rhs(timepoint = 0, work_data = work_data,
                                                               name_baseline_covariates = name_baseline_covariates,
                                                               name_time_covariates = name_time_covariates, 
                                                               name_regimen = reg, regimen = FALSE, 
                                                               Markov = Markov, constant_variables = constant_variables))})),
      if(length(name_censoring)>0){paste0(name_censoring,"_1"," ~ ", get_rhs(timepoint = 0, work_data = work_data,
                                                                             name_baseline_covariates = name_baseline_covariates,
                                                                             name_time_covariates = name_time_covariates, name_regimen = name_regimen,
                                                                             regimen = TRUE, Markov = Markov, constant_variables = constant_variables))} else{})
  } else{
    c(paste0(name_regimen,"_0"," ~ ", get_rhs(timepoint = 0, work_data = work_data,
                                              name_baseline_covariates = name_baseline_covariates,
                                              name_time_covariates = name_time_covariates, 
                                              name_regimen = name_regimen, regimen = FALSE, 
                                              Markov = Markov, constant_variables = constant_variables)),
      if(length(name_censoring)>0){paste0(name_censoring,"_1"," ~ ", get_rhs(timepoint = 0, work_data = work_data,
                                                                             name_baseline_covariates = name_baseline_covariates,
                                                                             name_time_covariates = name_time_covariates, 
                                                                             name_regimen = name_regimen, regimen = TRUE, 
                                                                             Markov = Markov, constant_variables = constant_variables))} else{})
  }
  if(time_horizon>1){
    gform = c(gform, unlist(lapply(1:(time_horizon-1),function(tk){
      if(independent_regimens){
        c(unlist(lapply(name_regimen, function(reg){paste0(reg,"_",tk," ~ ", 
                                                           get_rhs(timepoint = tk, work_data = work_data,
                                                                   name_baseline_covariates = name_baseline_covariates,
                                                                   name_time_covariates  = name_time_covariates, 
                                                                   name_regimen = reg, regimen = TRUE, 
                                                                   Markov = Markov, constant_variables = constant_variables))})),
          if(length(name_censoring)>0) {paste0(name_censoring,"_", tk+1, " ~ ",
                                               get_rhs(timepoint = tk, work_data = work_data,
                                                       name_baseline_covariates = name_baseline_covariates,
                                                       name_time_covariates = name_time_covariates,
                                                       name_regimen = name_regimen, regimen = TRUE,
                                                       Markov = Markov, constant_variables = constant_variables))}
          else {})
      } else{
        c(paste0(name_regimen,"_",tk," ~ ", get_rhs(timepoint = tk, work_data = work_data,
                                                    name_baseline_covariates = name_baseline_covariates,
                                                    name_time_covariates  = name_time_covariates, 
                                                    name_regimen = name_regimen, regimen = TRUE,
                                                    Markov = Markov, constant_variables = constant_variables)),
          if(length(name_censoring)>0) {paste0(name_censoring,"_", tk+1, " ~ ",
                                               get_rhs(timepoint = tk+1, work_data = work_data,
                                                       name_baseline_covariates = name_baseline_covariates,
                                                       name_time_covariates = name_time_covariates,
                                                       name_regimen = name_regimen, regimen = TRUE,
                                                       Markov = Markov, constant_variables = constant_variables))}
          else {})
      }
    })))
  }
  
  
  ## Note that A_k ~ V + L_0 + ... + L_(k-1) + A_(k-1) for k = 1,..., time_horizon, but A_0 ~ V + L_0
  ## i.e., regimen at baseline depends on additional baseline covariates, whereas in general, regimen depends
  ## on the previously observed covariates and regimen.
  ## The reason for this is we do not want to mistakenly assume that L_1 -> A_1 when in reality A_1 happens before L_1
  
  Qform <- unlist(lapply(1:time_horizon,function(tk){
    paste0("Q.kplus1 ~ ", get_rhs(timepoint = tk, work_data = work_data,
                                  name_baseline_covariates = name_baseline_covariates,
                                  name_time_covariates  = name_time_covariates, 
                                  name_regimen = name_regimen, regimen = TRUE,
                                  Markov = Markov, constant_variables = constant_variables))
  }))
  
  names(Qform)=paste0(name_outcome,"_",1:time_horizon)
  # names for treatment and censoring formulas
  names(gform) <- as.character(unlist(lapply(gform,function(x){strsplit(x," ~ ")[[1]][[1]]})))
  # Remove gformulas of variables not in data, e.g., if we exclude censor others at time point 0
  gform <- gform[names(gform) %in% names(work_data)]
  list(gform = gform, Qform = Qform)
}  
