### summary.runLtmle.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (10:19)
## Version:
## Last-Updated: jun  7 2024 (15:49) 
##           By: WJA3740
##     Update #: 14
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
summary.runLtmle <- function(object,time_horizon,regimen,outcome){
    if (missing(time_horizon)) time_horizon = object[[1]]$Ltmle_fit$info$time_horizon
    if (missing(regimen)) regimen = names(object)
    if (missing(outcome)) outcome = object[[1]]$Ltmle_fit$info$outcome
    out <- do.call(rbind,lapply(regimen,function(r){
        cbind(outcome = outcome,
              regimen = r,
              summary(object[[r]]$Ltmle_fit))
    }))
    out[]
}

######################################################################
### summary.runLtmle.R ends here


