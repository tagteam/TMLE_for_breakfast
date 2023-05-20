merge_data <- function(time_horizon,
                       regimen_data,
                       outcome_data, 
                       baseline_data,
                       timevar_data, 
                       name_outcome,                    
                       name_regimen,
                       name_censoring = NULL,
                       censored_label = "censored",
                       name_comp.event = NULL,
                       order_YC = TRUE,
                       test=FALSE){
  time_grid = 0:time_horizon
  K = length(time_grid)
  
  setkey(regimen_data[[name_regimen]],pnr)
  outcome_data <- outcome_data[[name_outcome]]
  setkey(outcome_data,pnr)
  regimen_name <- strsplit(names(regimen_data[[name_regimen]])[2],"_")[[1]][[1]]
  k <- as.numeric(strsplit(names(regimen_data[[name_regimen]])[length(names(regimen_data[[name_regimen]]))],"_")[[1]][[2]])
  wide_data=outcome_data[regimen_data[[name_regimen]]]
  #
  # deal with outcome/death/censored at index
  #
  ## D_0 = match(paste0(name_comp.event,"_",0),names(wide_data))
  ## C_0 = match(paste0(name_censoring,"_",0),names(wide_data))
  ## wide_data = wide_data[!(wide_data[[D_0]]%in%1)&!(wide_data[[C_0]]%in%censored_label)]
  #
  # adding the baseline covariates
  #
  wide_data=baseline_data[wide_data, on = c("pnr")]
  # subset and sort data
  work_data <- wide_data
  # add time covariates
  # first remove outcome if overlap
  if (length((outcome_overlap <- grep(paste0(name_outcome,"_"),names(timevar_data)))
             >0))
      timevar_data <- timevar_data[,-outcome_overlap,with=FALSE]
  setkey(timevar_data,pnr)
  work_data=timevar_data[work_data, on = c("pnr")]
  
  name_time_covariates = unlist(lapply(grep("_0",names(timevar_data),value=TRUE),function(x){substring(x,0,nchar(x)-2)}))
  name_baseline_covariates = setdiff(names(baseline_data),"pnr")
  
  if(order_YC == TRUE){
      work_data = work_data[,c("pnr", name_baseline_covariates,unlist(sapply(time_grid, function(timepoint){
          if(timepoint == 0){
              intersect(paste0(c(name_time_covariates,regimen_name),"_",timepoint),names(work_data))
          } else{
              if(timepoint != time_grid[K]){
                  intersect(paste0(c(name_outcome, name_time_covariates, name_comp.event, name_censoring, regimen_name),"_",timepoint),names(work_data))
              } else { #last time point
                  intersect(paste0(name_outcome,"_",timepoint),names(work_data))
              }
          }
      }))), with = FALSE]
  } else {
      work_data = work_data[,c("pnr", name_baseline_covariates,unlist(sapply(time_grid, function(timepoint){
          if(timepoint == 0){
              intersect(paste0(c(name_time_covariates, regimen_name),"_",timepoint),names(work_data))
          } else{
              if(timepoint != time_grid[K]){
                  intersect(paste0(c(name_censoring, name_outcome, name_time_covariates, name_comp.event, regimen_name),"_",timepoint), names(work_data))
              } else {
                  intersect(paste0(c(name_censoring, name_outcome),"_",timepoint),names(work_data))
              }
          }
      }))), with = FALSE]
  }
  list(data = work_data[],
       name_baseline_covariates = name_baseline_covariates,
       name_time_covariates = name_time_covariates,
       name_regimen = regimen_name)
}

