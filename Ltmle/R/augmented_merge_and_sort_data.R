merge_and_sort_data <- function(time_horizon,
                                regimen_data,
                                outcome_data, 
                                baseline_data,
                                timevar_data, 
                                name_outcome,                    
                                name_regimen,
                                name_censoring = NULL,
                                censored_label = "censored",
                                name_competing_risk = NULL){
    time_horizon = max(time_horizon)
    time_grid = 0:time_horizon
    K = length(time_grid)
    # the regimen may have two components (A and B) both are
    # identified based on the first (A)
    # here we select the specified element of a list of regimens
    ## if(is.list(outcome_data)){
    ## outcome_data <- outcome_data[[name_outcome]]
    ## }else{
    ## outcome_data <- as.data.table(outcome_data)}
    stopifnot(inherits(regimen_data,"data.table"))
    data.table::setkey(regimen_data,pnr)
    data.table::setkey(outcome_data,pnr)
    wide_data=outcome_data[regimen_data]
    # deal with outcome/death/censored at index
    Y_0 = match(paste0(name_outcome,"_",0),names(wide_data))
    D_0 = match(paste0(name_competing_risk,"_",0),names(wide_data))
    C_0 = match(paste0(name_censoring,"_",0),names(wide_data))
    if (!is.na(Y_0)){
        if(!is.na(D_0)&&!is.na(C_0)){wide_data = wide_data[!(wide_data[[Y_0]]%in%1)&!(wide_data[[D_0]]%in%1)&!(wide_data[[C_0]]%in%censored_label)]}
        if(!is.na(D_0)){wide_data = wide_data[!(wide_data[[Y_0]]%in%1)&!(wide_data[[D_0]]%in%1)]}
        if(!is.na(C_0)){wide_data = wide_data[!(wide_data[[Y_0]]%in%1)&!(wide_data[[C_0]]%in%censored_label)]}
    }else{
        if(!is.na(D_0)&&!is.na(C_0)){wide_data = wide_data[!(wide_data[[D_0]]%in%1)&!(wide_data[[C_0]]%in%censored_label)]}
        if(!is.na(D_0)){wide_data = wide_data[!(wide_data[[D_0]]%in%1)]}
        if(!is.na(C_0)){wide_data = wide_data[!(wide_data[[C_0]]%in%censored_label)]}
    }
    # adding the baseline covariates
  
  wide_data=baseline_data[wide_data,on = "pnr"]
  # subset and sort data
  work_data <- wide_data
  # add time covariates
  # first remove outcome if overlap
  if (!is.null(timevar_data)){
    if (length((outcome_overlap <- grep(paste0(name_outcome,"_"),names(timevar_data)))>0)){
      timevar_data <- timevar_data[,-outcome_overlap, with=FALSE]}
    data.table::setkey(timevar_data,pnr)
    work_data=timevar_data[work_data, on = c("pnr")]
  }
  
  name_time_covariates = unlist(lapply(grep("_0",names(timevar_data),value=TRUE),
                                       function(x){substring(x,0,nchar(x)-2)}))
  name_baseline_covariates = setdiff(names(baseline_data),"pnr")
  
  # sorting the variables for LTMLE
  work_data = work_data[,c("pnr", intersect(c(name_baseline_covariates,unlist(sapply(time_grid, function(timepoint){
    if(timepoint == 0){
      paste0(c(name_time_covariates, name_regimen),"_",timepoint)
    } else{
      if(timepoint != time_grid[K]){
        paste0(c(name_censoring, name_outcome, name_competing_risk, name_time_covariates, name_regimen),"_",timepoint)
      } else {
        paste0(c(name_censoring, name_outcome),"_",timepoint)
      }
    }
  }))), names(work_data))), with = FALSE]
  
  list(data = work_data[],
       name_baseline_covariates = name_baseline_covariates,
       name_time_covariates = name_time_covariates,
       name_regimen = name_regimen)
}

