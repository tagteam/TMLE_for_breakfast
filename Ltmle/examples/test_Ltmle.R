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
#try(setwd("~/TMLE_for_breakfast/Ltmle/"),silent = TRUE)
## copy of functions from CRAN package ltmle
ff <- sapply(list.files(path = "./R/",pattern = "R$",full.names = TRUE),source)
## our own augmentation files
ff <- sapply(list.files(path = "./augmentation/",pattern = "R$",full.names = TRUE),source)
# ff <- sapply(list.files(path = "~/registerTargets/registerTargets/exercises/Ltmle/",pattern = "R$",full.names = TRUE),source)

## loading simulation function (including prepared coefficients from Danish register data)
source("~/TMLE_for_breakfast/Ltmle/examples/get_lava_model.R")

# Create object from which we can simulate data
# -----------------------------------------------
time_horizon = 4
m=get_lava_model(time_horizon=time_horizon)


# Simulate data with 4 time intervals and 20000 patients
# ----------------------------------------------------------
set.seed(4)
sim_data <- as.data.table(sim(m, 10000))
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
                name_competing_risk = "Dead",
                time_horizon=time_horizon,
                outcome_data=sim_outcome,
                regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],
                baseline_data=sim_baseline_covariates,
                timevar_data=sim_time_covariates,
                SL.library="glm",
                abar = rep(1, time_horizon),
                verbose=TRUE)
f=do.call("Ltmle",x)

setwd("~/registerTargets/registerTargets/exercises/register_project/")
library(targets)
tar_load_everything() #note: run tar_make() first
run_Ltmle(
  name_outcome = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  outcome_data = survival_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(treat = rep(1, 4), control = rep(0, 4)),
  SL.library = "glm",
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 1,
  outcome_data = survival_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = 0, treat = 1),
  SL.library = c("SL.glm", "SL.glm.interaction"),
  SL.cvControl = list(V = 2),
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 1,
  outcome_data = survival_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = 0, treat = 1),
  SL.library = "glmnet",
  SL.cvControl = list(selector = "optimize", alpha =
                        0.5),
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 1,
  outcome_data = survival_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = 0, treat = 1),
  SL.library = c("SL.glm", "SL.glm.interaction", "SL.glmnet", "SL.ranger2"),
  #SL.ranger2 does not work
  SL.cvControl = list(V = 2),
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "mace",
  name_competing_risk = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  outcome_data = mace_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  SL.library = "glm",
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "mace",
  name_competing_risk = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  outcome_data = mace_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  iptw.only = TRUE,
  SL.library = "glm",
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "mace",
  name_competing_risk = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  outcome_data = mace_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  gcomp = TRUE,
  SL.library = "glm",
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "mace",
  name_competing_risk = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  sub_set = list(data = baseline_covariates[education == "High", .(pnr)]),
  outcome_data = mace_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  gcomp = TRUE,
  SL.library = "glm",
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "mace",
  name_competing_risk = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  sub_set = list(data = baseline_covariates[education == "High", .(pnr)]),
  outcome_data = mace_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  iptw.only = TRUE,
  SL.library = "glm",
  verbose = TRUE
)
run_Ltmle(
  name_outcome = "mace",
  name_competing_risk = "Dead",
  name_censoring = "Censored",
  censored_label = 0,
  time_horizon = 4,
  sub_set = list(data = baseline_covariates[education == "High", .(pnr)]),
  outcome_data = mace_outcome_data,
  regimen_data = list(Drug = regimen_data),
  baseline_data = baseline_covariates,
  timevar_data = time_covariates,
  abar = list(control = rep(0, 4), treat = rep(1, 4)),
  SL.library = "glm",
  verbose = TRUE
)
######################################################################
### test_Ltmle.R ends here
