### test_Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Aug  1 2023 (13:56) 
## Version: 
## Last-Updated: Oct 25 2024 (17:12) 
##           By: Thomas Alexander Gerds
##     Update #: 11
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(lava)
library(foreach)
library(data.table)
library(targets)
# loading Ltmle functions with augmentation
# -------------------------------------------
try(setwd("~/TMLE_for_breakfast/Ltmle/"),silent = TRUE)
try(setwd("~/research/Methods/TMLE_for_breakfast/Ltmle/"),silent = TRUE)
ff <- sapply(list.files(path = "./R/",pattern = "R$",full.names = TRUE),source)
## loading simulation function (including prepared coefficients from Danish register data)
try(source("~/research/Methods/TMLE_for_breakfast/Ltmle/examples/get_lava_model.R"),silent = TRUE)
# Create object from which we can simulate data
# -----------------------------------------------
time_horizon = 6
m=get_lava_model(time_horizon=time_horizon)
# Simulate data with 4 time intervals and 20000 patients
# ----------------------------------------------------------
set.seed(4)
sim_data <- as.data.table(sim(m, 40000))

## add person number
sim_data[,pnr:=1:.N]
## convert integer variables to numeric 
isINT=sapply(names(sim_data),function(n)is.integer(sim_data[[n]]))
sim_data[,(names(sim_data)[isINT]):=lapply(.SD, as.numeric), .SDcols = names(sim_data)[isINT]]
## extract baseline covariates
sim_baseline_covariates <- sim_data[,c("sexMale","agegroups","education","income","diabetes_duration","pnr"),with = FALSE]
## extract time varying covariates
sim_time_covariates <- sim_data[,grep(paste0(c("pnr","heart.failure","renal.disease","chronic.pulmonary.disease","any.malignancy","ischemic.heart.disease","myocardial.infarction","hypertension","stroke","bb","ccb","rasi","thiazid","loop","mra","copd_med"),collapse="|"),names(sim_data)), with = FALSE]
## extract outcome data
sim_outcome <- sim_data[,grep("pnr|dementia_|Censored|Dead", names(sim_data)), with = FALSE]

# Prepare Ltmle analysis
# -------------------------------------------
x=prepare_Ltmle(name_outcome="dementia",
                name_regimen="GLP1RA",
                name_id="pnr",
                name_censoring = "Censored",
                censored_label = "0",
                name_comp.event = "Dead",
                time_horizon=time_horizon,
                outcome_data=sim_outcome,
                regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],
                baseline_data=sim_baseline_covariates,
                timevar_data=sim_time_covariates,
                SL.library="glm",
                abar = rep(1, time_horizon),
                verbose=TRUE)
system.time(f<-do.call("Ltmle",x))
# with speedglm
# user  system elapsed 
# 25.757  46.239  11.291 
# with fastglm
#user  system elapsed 
# 8.829   8.232   7.638 

# survival without competing risks does not work right now 
x=prepare_Ltmle(name_outcome="dementia",
                name_regimen="GLP1RA",
                name_id = "pnr",
                name_censoring = "Censored",
                censored_label = "0",name_comp.event = NULL,
                time_horizon=time_horizon,
                outcome_data=sim_outcome[,grep("pnr|Censored|dementia", names(sim_outcome)), with = FALSE],
                regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],
                baseline_data=sim_baseline_covariates,
                timevar_data=sim_time_covariates,
                SL.library="glm",
                abar = rep(1, time_horizon),
                verbose=TRUE)
system.time(f<-do.call("Ltmle",x))


