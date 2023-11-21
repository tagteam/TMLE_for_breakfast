### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct 16 2023 (15:37) 
## Version: 
## Last-Updated: Oct 16 2023 (15:38) 
##           By: Thomas Alexander Gerds
##     Update #: 1
#----------------------------------------------------------------------
## 
### Commentary: 
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
### _targets.R ends here
