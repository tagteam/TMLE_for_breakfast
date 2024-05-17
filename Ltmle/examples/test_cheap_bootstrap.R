### test_cheap_bootstrap.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Feb 19 2024 (15:08) 
## Version: 
## Last-Updated: May  8 2024 (08:43) 
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
## copy of functions from CRAN package ltmle and own augmentation functions
ff <- sapply(list.files(path = "./R/",pattern = "R$",full.names = TRUE),source)
## run_ltmle etc
ff <- sapply(list.files(path = "~/research/Epidemi/dual-diabetes-treatment/incoming/",pattern = "R$",full.names = TRUE),try(source))


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
X=prepare_Ltmle(name_outcome="dementia",
                name_regimen="GLP1RA",
                name_censoring = "Censored",
                censored_label = "0",
                name_competing_risk = NULL,
                time_horizon=1:4,
                outcome_data=sim_outcome,
                regimen_data=sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE],
                baseline_data=sim_baseline_covariates,
                timevar_data=sim_time_covariates,
                SL.library="glm",
                iptw.only=FALSE,
                abar = rep(1,4),
                reduce = FALSE,
                verbose=TRUE)
Fit<-do.call("Ltmle",X) #f$cum.g has some NAs, yet we get causal effect estimate when removing the option iptw.only=TRUE


fit= poly_ltmle(name_outcome="dementia",
               time_horizon=c(2,4),
               regimen_data=list("GLP1RA" = sim_data[,grep("pnr|GLP1RA", names(sim_data)), with = FALSE]),
               outcome_data=list("dementia" = sim_outcome),
               baseline_data=sim_baseline_covariates,
               timevar_data=sim_time_covariates,
               B_bootstrap_samples=5,
               bootstrap_sample_size=ceiling(.632*NROW(sim_baseline_covariates)),
               SL.library="glm",
               censor_others=FALSE,
               Markov=NULL,
               verbose=FALSE,
               SL.cvControl=list(selector="undersmooth",alpha=0.5),
               gbounds=c(0,1))

######################################################################
### test_cheap_bootstrap.R ends here
