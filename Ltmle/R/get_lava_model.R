# library(lava)
# library(data.table)

get_lava_model <- function(time_horizon, name_baseline_covariates, name_time_covariates, name_regimen, name_outcome,
                           name_censoring = NULL, name_comp.event = NULL, censoring_model = NULL, comp.event_model = NULL,
                           Markov = NULL, prob_baseline, cov_model, prop_model, outcome_model, order_YC = FALSE){
  time_grid = 0:time_horizon
  K = length(time_grid)
  
  m <- lvm()
  
  ## Baseline variable
  distribution(m, name_baseline_covariates) <- binomial.lvm(p = prob_baseline)
  
  ## Time_varying variable for t = 0
  distribution(m, paste0(name_time_covariates,"_",0)) <- binomial.lvm()
  intercept(m, paste0(name_time_covariates,"_",0)) <- cov_model[[1]][1]
  regression(m, to = paste0(name_time_covariates,"_",0), from = name_baseline_covariates) <- cov_model[[1]][2:length(cov_model[[1]])]
  
  ## Treatment variable for t = 0
  
  for(i in 1:length(name_regimen)){
    distribution(m, paste0(name_regimen[[i]],"_",0)) <- binomial.lvm(link = "logit")
    intercept(m, paste0(name_regimen[[i]],"_",0)) <- prop_model[[i]][[1]][1]
    regression(m, to = paste0(name_regimen[[i]],"_",0), from = c(name_baseline_covariates, paste0(name_time_covariates,"_",0))) <- prop_model[[i]][[1]][2:length(prop_model[[i]][[1]])]
  }
  
  ## Variables for time t = 1,...,K-
  
  get_cov <- function(timepoint, name_baseline_covariates, name_time_covariates, name_regimen, regimen = TRUE, Markov){
    form = c(name_baseline_covariates)
    if(length(name_time_covariates[name_time_covariates%in%Markov])>0) {
      form = c(form, paste0(name_time_covariates[name_time_covariates%in%Markov], "_", max(0, (timepoint - 1))))
    }
    if(length(name_time_covariates[!(name_time_covariates%in%Markov)])>0){
      form = c(form, paste0(name_time_covariates[!(name_time_covariates%in%Markov)], "_", 0:max(0, (timepoint - 1))))
    }
    if(regimen == TRUE) {
      form = c(form, sapply(name_regimen, function(n) {paste0(n, "_", 0:max(0, (timepoint - 1)))}))
    }
    form[]
  }

  for(k in time_grid[-1]){
    if(order_YC){
      ## Outcome
      distribution(m, paste0(name_outcome,"_",k)) <- binomial.lvm()
      intercept(m, paste0(name_outcome,"_",k)) <- outcome_model[[k]][1]
      regression(m, to = paste0(name_outcome,"_",k), from = get_cov(timepoint = k, name_baseline_covariates = name_baseline_covariates, 
                                                                    name_time_covariates = name_time_covariates, name_regimen = name_regimen, 
                                                                    regimen = TRUE, Markov = Markov)) <- outcome_model[[k]][2:length(outcome_model[[k]])]
      ## Censoring
      if(length(name_censoring)>0){
        distribution(m, paste0(name_censoring,"_",k)) <- binomial.lvm()
        intercept(m, paste0(name_censoring,"_",k)) <- censoring_model[[k]][1]
        regression(m, to = paste0(name_censoring,"_",k), from = get_cov(timepoint = k, name_baseline_covariates = name_baseline_covariates, 
                                                                        name_time_covariates = name_time_covariates, name_regimen = name_regimen, 
                                                                        regimen = TRUE, Markov = Markov)) <- censoring_model[[k]][2:length(censoring_model[[k]])]
      }
    } else {
      ## Censoring
      if(length(name_censoring)>0){
        distribution(m, paste0(name_censoring,"_",k)) <- binomial.lvm()
        intercept(m, paste0(name_censoring,"_",k)) <- censoring_model[[k]][1]
        regression(m, to = paste0(name_censoring,"_",k), from = get_cov(timepoint = k, name_baseline_covariates = name_baseline_covariates, 
                                                                        name_time_covariates = name_time_covariates, name_regimen = name_regimen, 
                                                                        regimen = TRUE, Markov = Markov)) <- censoring_model[[k]][2:length(censoring_model[[k]])]
      }
      
      ## Outcome
      distribution(m, paste0(name_outcome,"_",k)) <- binomial.lvm()
      intercept(m, paste0(name_outcome,"_",k)) <- outcome_model[[k]][1]
      regression(m, to = paste0(name_outcome,"_",k), from = get_cov(timepoint = k, name_baseline_covariates = name_baseline_covariates, 
                                                                    name_time_covariates = name_time_covariates, name_regimen = name_regimen, 
                                                                    regimen = TRUE, Markov = Markov)) <- outcome_model[[k]][2:length(outcome_model[[k]])]
    }
    
    if(k != time_grid[K]){
        ## Competing event
        if(length(name_comp.event)>0){
            distribution(m, paste0(name_comp.event,"_",k)) <- binomial.lvm()
            intercept(m, paste0(name_comp.event,"_",k)) <- comp.event_model[[k]][1]
            regression(m, to = paste0(name_comp.event,"_",k), from = get_cov(timepoint = k, name_baseline_covariates = name_baseline_covariates, 
                                                                             name_time_covariates = name_time_covariates, name_regimen = name_regimen, 
                                                                             regimen = TRUE, Markov = Markov)) <- comp.event_model[[k]][2:length(comp.event_model[[k]])]
        }
        ## Time_varying covariate
        distribution(m, paste0(name_time_covariates,"_",k)) <- binomial.lvm()
        intercept(m, paste0(name_time_covariates,"_",k)) <- cov_model[[k]][1]
        regression(m, to = paste0(name_time_covariates,"_",k),
                   from = get_cov(timepoint = k, name_baseline_covariates = name_baseline_covariates, 
                                  name_time_covariates = name_time_covariates, name_regimen = name_regimen, 
                                  regimen = FALSE, Markov = Markov)) <- cov_model[[k+1]][2:length(cov_model[[k+1]])]
        ## Regimen
        for(i in 1:length(name_regimen)){
            distribution(m, paste0(name_regimen[[i]],"_",k)) <- binomial.lvm()
            intercept(m, paste0(name_regimen[[i]],"_",k)) <- prop_model[[i]][[k]][1]
            regression(m, to = paste0(name_regimen[[i]],"_",k),
                       from = get_cov(timepoint = k,
                                      name_baseline_covariates = name_baseline_covariates,
                                      name_time_covariates = name_time_covariates,
                                      name_regimen = name_regimen[[i]],
                                      regimen = TRUE,
                                      Markov = Markov)) <- prop_model[[i]][[k+1]][2:length(prop_model[[i]][[k+1]])]
        }
    }
  }
  
  if(time_horizon == 1) {name_comp.event = NULL}
  
  lvm_model <- list(model = m,
                    time_horizon = time_horizon,
                    baseline_covariates = name_baseline_covariates,
                    time_covariates = name_time_covariates,
                    regimen = name_regimen,
                    outcome = name_outcome,
                    censoring = name_censoring,
                    comp.event = name_comp.event,
                    order_YC = order_YC)
}

