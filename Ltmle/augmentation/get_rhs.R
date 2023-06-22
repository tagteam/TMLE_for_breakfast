## rhs of formulas used for GLM, g-formulas and Q-formulas
get_rhs <- function(timepoint, name_baseline_covariates, name_time_covariates, name_regimen, regimen = TRUE, Markov = NULL,
                    constant_variables = NULL){
  form = paste(setdiff(name_baseline_covariates,constant_variables), collapse = "+")
  
  # name_time_covariates = paste0(NULL,if(timepoint!=0){name_time_covariates}else{}) # Include if A_0 ~ V and C_1 ~ V only

  if(length(name_time_covariates[name_time_covariates%in%Markov])>0) {
      form = paste(form, "+", paste(setdiff(sapply(name_time_covariates[name_time_covariates%in%Markov],
                                                   function(ntc) {paste0(ntc, "_", max(0, (timepoint - 1)))}),
                                            constant_variables),
                                    collapse = " + "))
  }
  if(length(name_time_covariates[!(name_time_covariates%in%Markov)])>0){
      form = paste(form, "+", paste(setdiff(sapply(name_time_covariates[!(name_time_covariates%in%Markov)],
                                                   function(ntc) {paste0(ntc, "_", 0:max(0, (timepoint - 1)))}),
                                            constant_variables),
                                    collapse = " + "))
  }
  if(regimen == TRUE) {
      form = paste(form, "+", paste(setdiff(sapply(name_regimen, function(nt) {paste0(nt, "_", 0:max(0, (timepoint - 1)))}),
                                            constant_variables),
                                    collapse = " + "))
  }
  form[]
}

