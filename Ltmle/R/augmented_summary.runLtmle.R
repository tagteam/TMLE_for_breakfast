### summary.runLtmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (10:19) 
## Version: 
## Last-Updated: Jan  3 2024 (16:26) 
##           By: Thomas Alexander Gerds
##     Update #: 8
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
summary.runLtmle <- function(object,time_horizon,regimen,outcome){
    if (missing(time_horizon))
        time_horizon = names(object)
    else
        time_horizon = paste0("time_horizon_",time_horizon)
    if (missing(regimen)) regimen = names(object[[1]])
    if (missing(outcome)) outcome = gsub("_.*","",object[[1]][[1]]$Ynodes[[1]])
    do.call(rbind,lapply(time_horizon,function(tk){
        do.call(rbind,lapply(regimen,function(r){
            cbind(outcome = outcome,
                  time_horizon = gsub("time_horizon_","",tk),
                  regimen = r,
                  summary(object[[tk]][[r]]$Ltmle_fit))
        }))}))
}
######################################################################
### summary.runLtmle.R ends here
