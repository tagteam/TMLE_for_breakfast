### test_Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Aug  1 2023 (13:56) 
## Version: 
## Last-Updated: Aug  1 2023 (14:50) 
##           By: Thomas Alexander Gerds
##     Update #: 4
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
## copy of functions from CRAN package ltmle
ff <- sapply(list.files(path = "./R/",pattern = "R$",full.names = TRUE),source)
## our own augmentation files
ff <- sapply(list.files(path = "./augmentation/",pattern = "R$",full.names = TRUE),source)
# ff <- sapply(list.files(path = "~/registerTargets/registerTargets/exercises/Ltmle/",pattern = "R$",full.names = TRUE),source)

## loading simulation function (including prepared coefficients from Danish register data)
source("~/TMLE_for_breakfast/Ltmle/examples/get_lava_model.R")

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
## fix outcome after first occurrence
# for (i in 1:10){
#     for (j in ((i+1):10)){
#         set(sim_outcome,i=which(sim_outcome[[paste0("dementia_",i)]]==1),j=paste0("dementia_",j),value=1)
#         set(sim_outcome,i=which(sim_outcome[[paste0("dementia_",i)]]==1),j=paste0("Censored_",j),value=0)
#     }
# }

# Prepare Ltmle analysis
# -------------------------------------------
x=prepare_Ltmle(name_outcome="dementia",
                name_regimen="GLP1RA",
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
system.time(f<-do.call("Ltmle",x))

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
summary(do.call("Ltmle",x))

setwd("~/registerTargets/registerTargets/exercises/register_project/")
library(targets)
tar_load_everything() #note: run tar_make() first
try(setwd("~/TMLE_for_breakfast/Ltmle/"),silent = TRUE)
## copy of functions from CRAN package ltmle
ff <- sapply(list.files(path = "./R/",pattern = "R$",full.names = TRUE),source)
## our own augmentation files
ff <- sapply(list.files(path = "./augmentation/",pattern = "R$",full.names = TRUE),source)

# do survival? 
x=prepare_Ltmle(
  name_outcome = "mace",
  name_comp.event = "Dead",
  name_regimen = "Drug",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  outcome_data = mace_outcome_data,
  regimen_data = regimen_data,
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  SL.library = "glm",
  verbose = TRUE
) #how to do survival?
summary(do.call("Ltmle",x))

x=prepare_Ltmle(
  name_outcome = "mace",
  name_comp.event = "Dead",
  name_regimen = "Drug",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  outcome_data = mace_outcome_data,
  regimen_data = regimen_data,
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  SL.library = "glmnet",
  verbose = TRUE
)
summary(do.call("Ltmle",x))

x=prepare_Ltmle(
  name_outcome = "mace",
  name_comp.event = "Dead",
  name_regimen = "Drug",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  outcome_data = mace_outcome_data,
  regimen_data = regimen_data,
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  SL.library = "glm",
  iptw.only = TRUE,
  verbose = TRUE
)
summary(do.call("Ltmle",x)) #NAs?

x=prepare_Ltmle(
  name_outcome = "mace",
  name_comp.event = "Dead",
  name_regimen = "Drug",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  outcome_data = mace_outcome_data,
  regimen_data = regimen_data,
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  SL.library = "glm",
  gcomp = TRUE,
  verbose = TRUE
)
summary(do.call("Ltmle",x))

x=prepare_Ltmle(
  name_outcome = "mace",
  name_comp.event = "Dead",
  name_regimen = "Drug",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 2,
  outcome_data = mace_outcome_data,
  regimen_data = regimen_data,
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 2), treat = rep(1, 2)),
  SL.library = c("SL.glmnet","SL.glm"),
  verbose = TRUE
)
summary(do.call("Ltmle",x))


