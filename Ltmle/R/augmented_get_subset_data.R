## Subset rows and columns

get_subset_data <- function(work_data,
                            time_horizon,
                            subset_id = NULL,
                            subset_label = NULL,
                            name_baseline_covariates){
    time_horizon = max(time_horizon)
    time_grid = 0:time_horizon
    if(length(subset_id)>0){
        work_data = work_data[pnr%in%subset_id]
    }
    # label the variables that are constant in the subset data
    same = sapply(work_data, function(x){length(unique(x))==1})
    if(sum(same)>0){
        constant_variables <- names(work_data)[same]
    } else{
        constant_variables <- NULL}
    list(data = work_data[],
         subset_label = subset_label,
         name_baseline_covariates = name_baseline_covariates[name_baseline_covariates%in%names(work_data)],
         constant_variables = constant_variables)
}

