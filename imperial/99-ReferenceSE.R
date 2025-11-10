library("targets")
library("tidyverse")
library("data.table")
library("riskRegression")
# Manal (adapt the path to your computer):
#try(setwd("c:/git/TMLE_for_breakfast/imperial"),silent = TRUE)
rtmle_code <- "C:/git/rtmle-main/R/"
if (file.exists(rtmle_code)) tar_source(rtmle_code)
# Thomas:
try(setwd("~/research/Methods/TMLE_for_breakfast/imperial"),silent = TRUE)
try(library(rtmle),silent = TRUE)

tar_load(dummy_data)

set.seed(2025)

tau <- 5
x <- rtmle_init(intervals = tau,
                name_id = "ID",
                name_outcome = "Y",
                name_competing = "Dead",
                name_censoring = "Censored",
                censored_label = "censored")


x <- add_long_data(x,
                   outcome_data = dummy_data$outcome_data,
                   censored_data = dummy_data$censored_data,
                   competing_data = dummy_data$competing_data,
                   timevar_data = dummy_data$timevar_data)
x <- add_baseline_data(x, data = dummy_data$baseline_data)


## PREPARE THE DATA 
x <- long_to_wide(x,
                  intervals = seq(0, 930, 30.45*6),
                  # fun = list("HBC" = function(x){x},
                  #            "BMI" = function(x){x}),
                  start_followup_date = "start_followup_date")


x <- prepare_data(x)
#x$prepared_data |> View()

x <- protocol(x,name = "Always_Degludec_Never_Glargine",
              intervention = data.frame("Degludec" = factor(1,levels = c(0,1)),
                                        "Glargine" = factor(0,levels = c(0,1))))
x <- protocol(x,name = "Always_Glargine_Never_Degludec",
              intervention = data.frame("Degludec" = factor(0,levels = c(0,1)),
                                        "Glargine" = factor(1,levels = c(0,1))))

x <- target(x,name = "Outcome_risk",
            estimator = "tmle",
            protocols = c("Always_Degludec_Never_Glargine", 
                          "Always_Glargine_Never_Degludec"))

x$names$name_constant_variables <- c("Glargine_0", x$names$name_constant_variables)

# this is new
x <- model_formula(x,exclude_variables = c("Date","Glargine_0","start_followup_date"))

refProtocol <- list(Outcome_risk = "Always_Glargine_Never_Degludec")

x <- run_rtmle(x,
               refit = TRUE,
               learner = "learn_glmnet",
               time_horizon = 1:4)


## FROM here, save these parameters and we can shift to summary.rtmle() to debug
target_name <- "Outcome_risk"
protocol_name <- "Always_Degludec_Never_Glargine"
tp <- 4  
reference <- refProtocol
analysis <- "Main_analysis"
digits <- 1
object <- x

## SE of risk difference is the same as the reference group
summary(object = x,
        targets = "Outcome_risk", 
        reference = refProtocol) |> 
View()



