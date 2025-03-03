get_primary_analysis <- function(primary_outcomes,
                                 primary_treatment_regimens,
                                 primary_baseline_covariates,
                                 primary_time_covariates,
                                 SL.library = "glm", #glmnet
                                 time_horizon,
                                 allow_dropin=TRUE,
                                 verbose=FALSE){
    if (FALSE){
        tar_load(primary_outcomes)
        tar_load(dt_treatment_regimens_LTMLEprep)
        tar_load(dt_baseline_allvar_LTMLEprep)
        tar_load(dt_widencovar_LTMLEprep)
        tar_load(names_baseline_covariates)
        time_horizon=9
        censor_others=T
        treat="sglt2"
        SL.library="glmnet"
        primary_outcomes=primary_outcomes
        primary_treatment_regimens=dt_treatment_regimens_LTMLEprep
        primary_time_covariates=dt_widencovar_LTMLEprep
        primary_baseline_covariates=dt_baseline_allvar_LTMLEprep[,names_baseline_covariates,with=FALSE]
    }  
    names_primary_outcomes="mace"#names(primary_outcomes)
    result <- foreach(OUT = names_primary_outcomes,
                      .packages=c("data.table","ltmle","glmnet","foreach"))%do%{
                          cat(OUT,"\n")
                          foreach(treat = c("sglt2","dpp4"), .packages=c("data.table","ltmle","glmnet","foreach")) %do% {
                              cat(treat,"\n")
                              if (treat == "dpp4"){
                                  name_treat <- "dpp4_inhib"
                              } 
                            
                              if (treat == "sglt2"){
                                  name_treat <- "sglt2_inhib"
                              }
                              x <- prepare_Ltmle(name_outcome = OUT,
                                                 name_regimen = name_treat,
                                                 name_censoring = "Censored",
                                                 name_competing_risk = "Dead",
                                                 censored_label="censored",
                                                 time_horizon=time_horizon,
                                                 regimen_data=primary_treatment_regimens[[treat]],
                                                 outcome_data=primary_outcomes,
                                                 baseline_data=primary_baseline_covariates,
                                                 timevar_data=primary_time_covariates,
                                                 SL.library=SL.library,
                                                 gbounds=c(0,1),
                                                 abar = rep(1,time_horizon),
                                                 verbose=TRUE)
                              f <- do.call("Ltmle", x)
                              writexl::write_xlsx(summary(f),paste0("output/res_",name_treat,time_horizon,".xlsx"))
                              summary(f)
                          }
                      }
    ## stopCluster(cl)
    ## gc()
    names(result) <- names(primary_outcomes)[1:length(result)]
    result
}

