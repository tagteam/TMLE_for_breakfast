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

ModelData <- dummy_data

set.seed(2025)
analysis_horizon <- 6

AgeSub <- rtmle_init(
  time_grid = seq(0, 2000, 30.45*6),
  name_id = "id",
  name_outcome = "Y",
  name_competing = "Dead",
  name_censoring = "Censored",
  censored_label = "censored"
)

AgeSub <- add_long_data(AgeSub,
                        outcome_data = ModelData$outcome_data,
                        censored_data = ModelData$censored_data,
                        competing_data = ModelData$competing_data,
                        timevar_data = ModelData$timevar_data)

AgeSub <- add_baseline_data(AgeSub, 
                            data = ModelData$baseline_data |> 
                              select(-Date) |> 
                              setDT())


AgeSub <- long_to_wide(AgeSub, 
                       start_followup_date = "start_followup_date",
                       Degludec = list(Varibale = "Degludec", method = "event_interval"),
                       Glargine = list(Varibale = "Glargine", method = "event_interval"))


AgeSub <- prepare_rtmle_data(AgeSub)


AgeSub <- protocol(AgeSub, 
                   name = "Always_Degludec_Never_Glargine",
                   intervention = data.frame(
                     node = AgeSub$intervention_nodes,
                     "Degludec" = factor(1,levels = c(0,1)),
                     "Glargine" = factor(0,levels = c(0,1))))
                   
AgeSub <- protocol(AgeSub, 
                   name = "Always_Glargine_Never_Degludec",
                   intervention = data.frame(
                     node = AgeSub$intervention_nodes,
                     "Degludec" = factor(0,levels = c(0,1)),
                     "Glargine" = factor(1,levels = c(0,1))))

AgeSub <- target(AgeSub,
                 name = "Outcome_risk",
                 estimator = "tmle",
                 protocols = c("Always_Degludec_Never_Glargine", 
                               "Always_Glargine_Never_Degludec"))


AgeSub <- model_formula(AgeSub,
                        verbose = FALSE, 
                        exclude_rules = list("*" = "Glargine_0"))

refProtocol2 <- list(Outcome_risk = "Always_Glargine_Never_Degludec")


## STRATA FOR THE SUBGROUP: AGE
AGEStrata <- list(list(label = "AGE",
                       append = TRUE,
                       id = AgeSub$prepared_data[Age >= 50, id],
                       variable = "Subgroup",
                       level = "Age >= 50"),
                  list(label = "AGE",
                       append = TRUE,
                       id = AgeSub$prepared_data[Age < 50, id],
                       variable = "Subgroup",
                       level = "Age < 50"))

# subset analysis
AgeSub$estimate$AGE <- NULL
AgeSub <- run_rtmle(AgeSub,
                    learner = "learn_glmnet", 
                    time_horizon = 1:analysis_horizon,
                    verbose = FALSE,
                    subsets = AGEStrata,
                    keep_influence = TRUE)



AgeAnalysis <- summary(AgeSub, analysis = "AGE", reference = refProtocol2)
