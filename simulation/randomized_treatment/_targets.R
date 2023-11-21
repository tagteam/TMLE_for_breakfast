### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Sep 12 2023 (11:03) 
## Version: 
## Last-Updated: Sep 27 2023 (08:55) 
##           By: Thomas Alexander Gerds
##     Update #: 27
#----------------------------------------------------------------------
## 
### Commentary:
##
## We consider a (Y,A,W) scenario where we know that A is independent of W
## and investigate if using this knowledge for the initial estimator of the
## propensity score as input to the tmle is a good or a bad idea
## in terms of efficiency. 
## 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
try(setwd("~/research/Methods/TMLE_for_breakfast/simulation/randomized_treatment/"),silent = 1L)
try(setwd("C:/Users/zjl310/Desktop/TMLE_for_breakfast/TMLE_for_breakfast/simulation/randomized_treatment/"))
for (f in list.files("C:/Users/zjl310/Desktop/TMLE_for_breakfast/TMLE_for_breakfast/Ltmle/augmentation/",pattern = "R$",full.names = TRUE)){source(f)}
# for (f in list.files("C:/Users/zjl310/Desktop/TMLE_for_breakfast/TMLE_for_breakfast/Ltmle/R/",pattern = "R$",full.names = TRUE)){source(f)}
for (f in list.files("C:/Users/zjl310/Desktop/TMLE_for_breakfast/TMLE_for_breakfast/simulation/randomized_treatment/functions/",pattern = "R$",full.names = TRUE)){source(f)}
library(targets)
library(tarchetypes)
library(lava)
library(devtools)
## library(drtmle)
library(data.table)
library(tibble)
library(augmentedLtmle)
## library(lmtp)

VALUES <- value_fct(n = 500, 
                    outcome_prevalence = c(0.1,0.1),
                    death_prevalence = c(0.1),
                    censoring_probability = c(0.2),
                    effects = list(
                      # effects are W,L_0,A_0
                      "A_1" = c(0.3,0.02,6),
                      "B_1" = c(0.3,0.02,-6),
                      "L_1" = c(0.3,2,-0.5),
                      "Y_1" = c(0.3,2,-0.2),
                      "Censored_1" = c(0,0,0),
                      "Censored_2" = c(0,0,0),
                      "Dead_1" = c(0,0,0),
                      # effects are W,L_0,A_0,L_1,A_1,B_1
                      "Y_2" = list(c(0.1,2,-0.1,2,-0.2,0.3))))
REPS <- 500

targets <- list(
  tar_map_rep(dgm_target_parameter, 
              {dlarge = simulate_data(n = 100000,
                                     outcome_prevalence = outcome_prevalence,
                                     death_prevalence = death_prevalence,
                                     censoring_probability = censoring_probability,
                                     regimen = TRUE,
                                     effects = list(
                                       "A_1" = A_1_effects,
                                       "B_1" = B_1_effects,
                                       "L_1" = L_1_effects,
                                       "Y_1" = Y_1_effects,
                                       "Censored_1" = Censored_1_effects,
                                       "Censored_2" = Censored_2_effects,
                                       "Dead_1" = Dead_1_effects,
                                       "Y_2" = Y_2_effects))
              dlarge[,.(Estimate = c("True"), Risk = c(mean(Y_2)))]},
              values = unique(VALUES[-1]),
              names = tidyselect::any_of("scenario"),
              batches = 1,
              reps = 1),
  tar_map_rep(simulated_estimator,
              estimate_target_parameter(n = n,
                                        outcome_prevalence = outcome_prevalence,
                                        death_prevalence = death_prevalence,
                                        censoring_probability = censoring_probability,
                                        regimen = TRUE,
                                        effects = list(
                                          "A_1" = A_1_effects,
                                          "B_1" = B_1_effects,
                                          "L_1" = L_1_effects,
                                          "Y_1" = Y_1_effects,
                                          "Censored_1" = Censored_1_effects,
                                          "Censored_2" = Censored_2_effects,
                                          "Dead_1" = Dead_1_effects,
                                           "Y_2" = Y_2_effects)),
              values = VALUES,
              names = tidyselect::any_of("scenario"),
              batches = 1,
              reps = REPS),
  tar_map_rep(simulated_estimator_B0,
              estimate_target_parameter(n = n,
                                        outcome_prevalence = outcome_prevalence,
                                        death_prevalence = death_prevalence,
                                        censoring_probability = censoring_probability,
                                        regimen = TRUE,
                                        effects = list(
                                          "A_1" = A_1_effects,
                                          "B_1" = B_1_effects,
                                          "L_1" = L_1_effects,
                                          "Y_1" = Y_1_effects,
                                          "Censored_1" = Censored_1_effects,
                                          "Censored_2" = Censored_2_effects,
                                          "Dead_1" = Dead_1_effects,
                                          "Y_2" = Y_2_effects), B_0 = TRUE),
              values = VALUES,
              names = tidyselect::any_of("scenario"),
              batches = 1,
              reps = REPS),
  tar_map_rep(simulated_estimator_combine,
              estimate_target_parameter(n = n,
                                        outcome_prevalence = outcome_prevalence,
                                        death_prevalence = death_prevalence,
                                        censoring_probability = censoring_probability,
                                        regimen = TRUE,
                                        effects = list(
                                          "A_1" = A_1_effects,
                                          "B_1" = B_1_effects,
                                          "L_1" = L_1_effects,
                                          "Y_1" = Y_1_effects,
                                          "Censored_1" = Censored_1_effects,
                                          "Censored_2" = Censored_2_effects,
                                          "Dead_1" = Dead_1_effects,
                                          "Y_2" = Y_2_effects),
                                        combine = TRUE),
              values = VALUES,
              names = tidyselect::any_of("scenario"),
              batches = 1,
              reps = REPS),
  tar_map_rep(simulated_estimator_gbounds,
              estimate_target_parameter(n = n,
                                        outcome_prevalence = outcome_prevalence,
                                        death_prevalence = death_prevalence,
                                        censoring_probability = censoring_probability,
                                        regimen = TRUE,
                                        effects = list(
                                          "A_1" = A_1_effects,
                                          "B_1" = B_1_effects,
                                          "L_1" = L_1_effects,
                                          "Y_1" = Y_1_effects,
                                          "Censored_1" = Censored_1_effects,
                                          "Censored_2" = Censored_2_effects,
                                          "Dead_1" = Dead_1_effects,
                                          "Y_2" = Y_2_effects),
                                        combine = TRUE, gbounds = c(0,1)),
              values = VALUES,
              names = tidyselect::any_of("scenario"),
              batches = 1,
              reps = REPS),
  tar_target(summary_simulation, {
    summarize_simulation(simulated_estimator, dgm_target_parameter)
  }),
  tar_target(summary_simulation_B0, {
    summarize_simulation(simulated_estimator_B0, dgm_target_parameter)
  }),
  tar_target(summary_simulation_combine, {
    summarize_simulation(simulated_estimator_combine, dgm_target_parameter)
  }),
  tar_target(summary_simulation_gbounds, {
    summarize_simulation(simulated_estimator_gbounds, dgm_target_parameter)
  })
)

list(targets)


# list(
#   tar_target(dgm_target_parameter,{
#     dlarge = simulate_data(n = 100000,treatment_effect = 0.1,covariate_effect = c(0.7,0))
#     dlarge[,.(Estimate = c("True"),
#               Risk_treated = c(mean(Y_treated)),
#               Risk_untreated = c(mean(Y_untreated)),
#               ATE = c(mean(Y_treated)-mean(Y_untreated)))]}),
#   tar_rep(simulated_estimator,
#           estimate_target_parameter(),
#           batches = 1,
#           reps = 20
#   )
# )


######################################################################
###_targets.R ends here
