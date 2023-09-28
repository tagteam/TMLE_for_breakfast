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
try(setwd("~/research/Methods/TMLE_for_breakfast/simulation/randomized_treatment/"))
for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){source(f)}
library(targets)
library(tarchetypes)
library(lava)
## library(drtmle)
library(data.table)
library(augmentedLtmle)
## library(lmtp)
list(
    tar_target(dgm_target_parameter,{
        dlarge = simulate_data(n = 100000,treatment_effect = 0.1,covariate_effect = c(0.7,0))
        dlarge[,.(Estimate = c("True"),
                  Risk_treated = c(mean(Y_treated)),
                  Risk_untreated = c(mean(Y_untreated)),
                  ATE = c(mean(Y_treated)-mean(Y_untreated)))]}),
    tar_rep(simulated_estimator,
            estimate_target_parameter(),
            batches = 1,
            reps = 2000
            )
)

######################################################################
###_targets.R ends here
