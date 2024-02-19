### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Feb  6 2024 (06:52) 
## Version: 
## Last-Updated: Feb  7 2024 (10:34) 
##           By: Thomas Alexander Gerds
##     Update #: 20
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
library(lava)
library(foreach)
library(data.table)
try(setwd("~/research/Methods/TMLE_for_breakfast/Ltmle/examples"),silent = TRUE)
try(source("get_lava_model.R"),silent = TRUE)
tar_source("../R/")
list(
    tar_target(name = test_lvm,command = {
        m=get_lava_model(time_horizon=6)
    }),
    tar_target(name = sim_data,
               command = {
                   sim_data <- as.data.table(sim(test_lvm, 4000))
                   ## add person number
                   sim_data[,pnr:=1:.N]
                   ## convert integer variables to numeric 
                   isINT=sapply(names(sim_data),function(n)is.integer(sim_data[[n]]))
                   sim_data[,(names(sim_data)[isINT]):=lapply(.SD, as.numeric), .SDcols = names(sim_data)[isINT]]
                   ## sim_data[Censored_1 == 0,Dead_1 := NA] 
                   sim_data[]
               }),
    tar_target(name = test_fit,command = {
        ## extract baseline covariates
        sim_baseline_covariates <- sim_data[,c("sexMale","agegroups","education","income","diabetes_duration","pnr"),with = FALSE]
        ## extract time varying covariates
        sim_time_covariates <- sim_data[,grep(paste0(c("pnr","heart.failure","renal.disease","chronic.pulmonary.disease","any.malignancy","ischemic.heart.disease","myocardial.infarction","hypertension","stroke","bb","ccb","rasi","thiazid","loop","mra","copd_med"),collapse="|"),names(sim_data)), with = FALSE]
        ## extract outcome data
        sim_outcome <- sim_data[,grep("pnr|dementia_|Censored|Dead", names(sim_data)), with = FALSE]
        x=prepare_Ltmle(name_outcome="dementia",name_regimen="GLP1RA",name_censoring = "Censored",censored_label = "0",name_competing_risk = "Dead",time_horizon=c(1,2),outcome_data=sim_outcome,regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],baseline_data=sim_baseline_covariates,timevar_data=sim_time_covariates,SL.library="glm",abar = rep(1, 2),verbose=FALSE)
        x23=prepare_Ltmle(name_outcome="dementia",name_regimen="GLP1RA",name_censoring = "Censored",censored_label = "0",name_competing_risk = "Dead",time_horizon=c(2,3),outcome_data=sim_outcome,regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],baseline_data=sim_baseline_covariates,timevar_data=sim_time_covariates,SL.library="glm",abar = rep(1, 3),verbose=FALSE)
        x234=prepare_Ltmle(name_outcome="dementia",name_regimen="GLP1RA",name_censoring = "Censored",censored_label = "0",name_competing_risk = "Dead",time_horizon=c(2,3,4),outcome_data=sim_outcome,regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],baseline_data=sim_baseline_covariates,timevar_data=sim_time_covariates,SL.library="glm",abar = rep(1, 4),verbose=FALSE)
        x3=prepare_Ltmle(name_outcome="dementia",name_regimen="GLP1RA",name_censoring = "Censored",censored_label = "0",name_competing_risk = "Dead",time_horizon=c(3),outcome_data=sim_outcome,regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],baseline_data=sim_baseline_covariates,timevar_data=sim_time_covariates,SL.library="glm",abar = rep(1, 3),verbose=FALSE)
        x1=prepare_Ltmle(name_outcome="dementia",name_regimen="GLP1RA",name_censoring = "Censored",censored_label = "0",name_competing_risk = "Dead",time_horizon=c(1),outcome_data=sim_outcome,regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],baseline_data=sim_baseline_covariates,timevar_data=sim_time_covariates,SL.library="glm",abar = rep(1, 1),verbose=FALSE)
        x2=prepare_Ltmle(name_outcome="dementia",name_regimen="GLP1RA",name_censoring = "Censored",censored_label = "0",name_competing_risk = "Dead",time_horizon=c(2),outcome_data=sim_outcome,regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],baseline_data=sim_baseline_covariates,timevar_data=sim_time_covariates,SL.library="glm",abar = rep(1, 2),verbose=FALSE)
        f <- do.call("Ltmle",x)
        f1 <- do.call("Ltmle",x1)
        f2 <- do.call("Ltmle",x2)
        f3 <- do.call("Ltmle",x3)
        f23 <- do.call("Ltmle",x23)
        f234 <- do.call("Ltmle",x234)
        b = readRDS("~/tmp/b.rds")
        a = readRDS("~/tmp/a.rds")
        do.call("FixedTimeTMLE", b)$IC
        do.call("FixedTimeTMLE", a)$IC
    })
)

######################################################################
### _targets.R ends here
