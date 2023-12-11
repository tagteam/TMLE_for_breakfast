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
## loading simulation function (including prepared coefficients from Danish register data)
source("./examples/get_lava_model.R")

# Create object from which we can simulate data
# -----------------------------------------------
m=get_lava_model(time_horizon=8)


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
                time_horizon=8,
                outcome_data=sim_outcome,
                regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],
                baseline_data=sim_baseline_covariates,
                timevar_data=sim_time_covariates,
                SL.library="glm",
                abar = rep(1, 8),
                verbose=TRUE)
# run Ltmle
system.time(f1<-do.call("Ltmle",x)) #fastglm
#user  system elapsed 
#13.936  11.533  12.263 

system.time(f2<-do.call("Ltmle",x)) #speedglm
#   user  system elapsed 
#44.231  80.819  19.363 
#summary(f)

> summary(f1)
Target_parameter Estimator   estimate     std.err      lower      upper       pvalue
1:        Risk(A=1)      tmle 0.01829853 0.004929428 0.00863703 0.02796003 0.0002055465
> summary(f2)
Target_parameter Estimator   estimate     std.err      lower      upper       pvalue
1:        Risk(A=1)      tmle 0.01829853 0.004929428 0.00863703 0.02796003 0.0002055465

## compare the estimates from the summary of f1 and f2




######################################################################
### test_Ltmle.R ends here
