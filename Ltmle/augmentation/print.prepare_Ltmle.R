### print.prepare_Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 19 2023 (15:57) 
## Version: 
## Last-Updated: May 19 2023 (20:41) 
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

print.prepare_Ltmle <- function(x,...){
    cat("Prepared data for analysis with Ltmle\n")
    censored = x$data[,lapply(.SD,function(x){sum(x == "censored",na.rm = TRUE)}),.SDcols = x$Cnodes]
    if (length(x$info$comprisk)>0)
        comprisk = x$data[,lapply(.SD,sum,na.rm = TRUE),.SDcols = grep(x$info$comprisk,names(x$data),value = TRUE)]
    outcomes = x$data[,lapply(.SD,sum,na.rm = TRUE),.SDcols = x$Ynodes]
    print(censored)
    print(comprisk)
    print(outcomes)
}
pl

######################################################################
### print.prepare_Ltmle.R ends here
