### print.prepare_Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 19 2023 (15:57) 
## Version: 
## Last-Updated: May 20 2023 (09:23) 
##           By: Thomas Alexander Gerds
##     Update #: 13
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
print.prepare_Ltmle <- function(x,...){
    cat("Prepared data for analysis with Ltmle. Sum of treated and number of events by time: \n\n")
    outcomes = x$data[,lapply(.SD,sum,na.rm = TRUE),.SDcols = x$Ynodes]
    outcomes = melt(outcomes,measure.vars = names(outcomes),variable.name = "Time",value.name = "Outcome")
    outcomes[,Time := sapply(strsplit(as.character(Time),"_"),"[",2)]
    treated = x$data[,lapply(.SD,sum,na.rm = TRUE),.SDcols = x$Anodes]
    treated = melt(treated,measure.vars = names(treated),variable.name = "Time",value.name = "Treatment")
    treated[,Time := sapply(strsplit(as.character(Time),"_"),"[",2)]
    X = merge(treated,outcomes,by = "Time",all = TRUE)
    if (length(x$Cnodes)>0){
        censored = x$data[,lapply(.SD,function(x){sum(x == "censored",na.rm = TRUE)}),.SDcols = x$Cnodes]
        censored = melt(censored,measure.vars = names(censored),variable.name = "Time",value.name = "Censored")
        censored[,Time := sapply(strsplit(as.character(Time),"_"),"[",2)]
        X = censored[X,on = "Time"]
        X[Time == 0,Censored := 0]
        X[Time == max(Time),Censored := NA]
    }
    if (length(x$info$comprisk)>0){
        comprisk = x$data[,lapply(.SD,sum,na.rm = TRUE),.SDcols = grep(x$info$comprisk,names(x$data),value = TRUE)]
        comprisk = melt(comprisk,measure.vars = names(comprisk),variable.name = "Time",value.name = "CompetingEvents")
        comprisk[,Time := sapply(strsplit(as.character(Time),"_"),"[",2)]
        X = comprisk[X,on = "Time"]
        X[Time == 0,CompetingEvents := 0]
    }
    X[Time == 0,Outcome := 0]
    X[,NumberAtrisk := rep(NROW(x$data),length(Time))-(cumsum(CompetingEvents)+cumsum(Censored)+cumsum(Outcome))]
    setcolorder(X,c("Time","NumberAtrisk","Treatment","Outcome","CompetingEvents","Censored"))
    print(X)
    invisible(X)
}

######################################################################
### print.prepare_Ltmle.R ends here
