### test_Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Aug  1 2023 (13:56) 
## Version: 
## Last-Updated: Jan  3 2024 (14:53) 
##           By: Thomas Alexander Gerds
##     Update #: 5
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
# loading Ltmle functions with augmentation
# -------------------------------------------
try(setwd("~/TMLE_for_breakfast/Ltmle/"),silent = TRUE)
try(setwd("~/research/Methods/TMLE_for_breakfast/Ltmle/"),silent = TRUE)
## copy of functions from CRAN package ltmle
ff <- sapply(list.files(path = "./R/",pattern = "R$",full.names = TRUE),source)
## our own augmentation files
ff <- sapply(list.files(path = "./augmentation/",pattern = "R$",full.names = TRUE),source)
# ff <- sapply(list.files(path = "~/registerTargets/registerTargets/exercises/Ltmle/",pattern = "R$",full.names = TRUE),source)

## loading simulation function (including prepared coefficients from Danish register data)
source("./examples/get_lava_model.R")

# Create object from which we can simulate data
# -----------------------------------------------
time_horizon = 4
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
## fix outcome after first occurrence
# for (i in 1:10){
#     for (j in ((i+1):10)){
#         set(sim_outcome,i=which(sim_outcome[[paste0("dementia_",i)]]==1),j=paste0("dementia_",j),value=1)
#         set(sim_outcome,i=which(sim_outcome[[paste0("dementia_",i)]]==1),j=paste0("Censored_",j),value=0)
#     }
# }

# one time point is broken
x=prepare_Ltmle(name_outcome="dementia",
                name_regimen="GLP1RA",
                name_censoring = "Censored",
                censored_label = "0",
                name_comp.event = "Dead",
                time_horizon=1,
                outcome_data=sim_outcome,
                regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],
                baseline_data=sim_baseline_covariates,
                timevar_data=sim_time_covariates,
                SL.library="glm",
                abar = 1,
                verbose=TRUE)
f<-do.call("Ltmle",x)

# iptw option is broken
x=prepare_Ltmle(name_outcome="dementia",
                name_regimen="GLP1RA",
                name_censoring = "Censored",
                censored_label = "0",
                name_comp.event = "Dead",
                time_horizon=4,
                outcome_data=sim_outcome,
                regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],
                baseline_data=sim_baseline_covariates,
                timevar_data=sim_time_covariates,
                SL.library="glm",
                iptw.only=TRUE,
                abar = rep(1,4),
                verbose=TRUE)
f<-do.call("Ltmle",x) #f$cum.g has some NAs, yet we get causal effect estimate when removing the option iptw.only=TRUE
summary(f)

# It can't do survival
# set Dead_0, ... Dead_k to 0
sim_outcome[,paste0("Dead_",0:time_horizon):=0]
x=prepare_Ltmle(name_outcome="dementia",
                name_regimen="GLP1RA",
                name_censoring = "Censored",
                censored_label = "0",
                name_comp.event = "Dead",
                time_horizon=4,
                outcome_data=sim_outcome,
                regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],
                baseline_data=sim_baseline_covariates,
                timevar_data=sim_time_covariates,
                SL.library="glm",
                abar = rep(1,4),
                verbose=TRUE)
summary(do.call("Ltmle",x))
