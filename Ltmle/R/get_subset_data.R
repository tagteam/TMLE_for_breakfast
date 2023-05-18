## Subset rows and columns

get_subset_data <- function(work_data, time_horizon, 
                            subset_id = NULL, subset_label = "No subset",
                            name_outcome,
                            name_regimen,
                            name_baseline_covariates,
                            name_time_covariates,
                            name_censoring = NULL,
                            name_comp.event = NULL,
                            Markov = NULL,
                            order_YCD = TRUE){
  
  time_grid = 0:time_horizon
  K = length(time_grid)
  
  if(length(subset_id)>0){
    work_data = work_data[pnr%in%subset_id]
  }
  
  Yk = c(paste0(name_outcome,"_",time_horizon))
  
  same = sapply(work_data, function(.col){all(is.na(.col)) || all(.col[1L] == .col)})
  # if(same[Yk] == TRUE) warning("The Y node at the time horizon appears to be constant or all NA")

  # work_data = work_data[, !same, with = FALSE] ## Remove constant variables

  ## Test whether we have excluded any time varying covariates
  
  test_cov <- c(t(sapply(time_grid[-K], function(timepoint){paste0(name_time_covariates,"_",timepoint)})))%in%names(work_data)
  index_cov = c(NA)
  for(i in 1:length(name_time_covariates)){
    index_cov[i] = (sum(test_cov[(((i-1)*(K-1)+1):(i*(K-1)))])>0)
  }
  
  test_reg <- c(t(sapply(time_grid[-K], function(timepoint){paste0(name_regimen,"_",timepoint)})))%in%names(work_data)
  index_reg = c(NA)
  for(i in 1:length(name_regimen)){
    index_reg[i] = (sum(test_reg[(((i-1)*(K-1)+1):(i*(K-1)))])>0)
  }
  
  list(data = work_data[],
       subset_label = subset_label,
       outcome = name_outcome,
       regimen = name_regimen[index_reg],
       baseline = name_baseline_covariates[name_baseline_covariates%in%names(work_data)],
       timevar = name_time_covariates[index_cov],
       Markov = name_time_covariates[(name_time_covariates%in%Markov)&index_cov],
       constant_variables = names(which(same == TRUE)),
       constant_message = ifelse(length(c(name_regimen[!index_reg], name_baseline_covariates[!(name_baseline_covariates%in%names(work_data))],
                                            name_time_covariates[!index_cov]))>0, 
                                   paste0("The following variables are constant and therefore removed: ",
                                          name_regimen[!index_reg], if(length(name_regimen[!index_reg])>0){", "} else{},
                                          name_baseline_covariates[!(name_baseline_covariates%in%names(work_data))],
                                          if(length(name_baseline_covariates[!(name_baseline_covariates%in%names(work_data))])>0){", "} else{},
                                          name_time_covariates[!index_cov]), "No message"))
}

