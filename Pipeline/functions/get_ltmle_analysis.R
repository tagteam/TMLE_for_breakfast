### get_ltmle_analysis.R --- 
#----------------------------------------------------------------------
## author: 
## created: apr 16 2024 (12:24) 
## Version: 
## last-updated: apr 16 2024 (12:26) 
##           By: WJA3740
##     Update #: 1
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_ltmle_analysis <- function(outcome,treatment,primary_treatment_regimens,primary_outcomes,primary_time_covariates,SL.library,time_horizon){
    x <- prepare_Ltmle(name_outcome = outcome,
                       name_regimen = treatment,
                       name_censoring = "Censored",
                       name_competing_risk = "Dead",
                       censored_label="censored",
                       time_horizon=time_horizon,
                       regimen_data=primary_treatment_regimens,
                       outcome_data=primary_outcomes,
                       baseline_data=primary_baseline_covariates,
                       timevar_data=primary_time_covariates,
                       SL.library=SL.library,
                       gbounds=c(0,1),
                       abar = rep(1,time_horizon),
                       verbose=TRUE)
    f <- do.call("Ltmle", x)
    summary(f)
}


#----------------------------------------------------------------------
### get_ltmle_analysis.R ends here
