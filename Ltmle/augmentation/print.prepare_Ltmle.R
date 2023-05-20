### print.prepare_Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 19 2023 (15:57) 
## Version: 
## Last-Updated: May 20 2023 (11:41) 
##           By: Thomas Alexander Gerds
##     Update #: 31
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
print.prepare_Ltmle <- function(x,...){
    cat("\nPrepared data for analysis with Ltmle.\n")
    order = ifelse(x$info$order_YC,"The events variables are ordered by: outcome, censoring",
                   "The event variables are ordered by: censoring, outcome")
    cat(order,"\n")
    cat("Sum of treated and number of events by time: \n\n")
    outcomes = x$data[,lapply(.SD,sum,na.rm = TRUE),.SDcols = x$Ynodes]
    outcomes = melt(outcomes,measure.vars = names(outcomes),variable.name = "Time",value.name = "Outcome")
    outcomes[,Time := sapply(strsplit(as.character(Time),"_"),"[",2)]
    treated = x$data[,lapply(.SD,sum,na.rm = TRUE),.SDcols = x$Anodes]
    treated = melt(treated,measure.vars = names(treated),variable.name = "Time",value.name = "Treatment")
    treated[,Time := sapply(strsplit(as.character(Time),"_"),"[",2)]
    X = merge(treated,outcomes,by = "Time",all = TRUE)
    X[Time == 0,Outcome := 0]
    X[,NumberAtrisk := rep(NROW(x$data),length(Time))-cumsum(Outcome)]
    if (length(x$Cnodes)>0){
        censored = x$data[,lapply(.SD,function(x){sum(x == "censored",na.rm = TRUE)}),.SDcols = x$Cnodes]
        censored = melt(censored,measure.vars = names(censored),variable.name = "Time",value.name = "Censored")
        censored[,Time := sapply(strsplit(as.character(Time),"_"),"[",2)]
        X = censored[X,on = "Time"]
        if (x$info$order_YC){
            X[Time == 0,Censored := 0]
        }else{
            X[,Censored := c(Censored[-1],NA)]
        }
        X[Time == max(Time),Censored := NA]
        X[,NumberAtrisk := NumberAtrisk-cumsum(Censored)]
    }
    has_competing = (length(x$info$comprisk)>0
        &&
        length(grep(paste0("^",x$info$comprisk,"_"),x$info$Lnodes)>0))
    if (has_competing){
        comprisk = x$data[,lapply(.SD,sum,na.rm = TRUE),.SDcols = grep(x$info$comprisk,names(x$data),value = TRUE)]
        comprisk = melt(comprisk,measure.vars = names(comprisk),variable.name = "Time",value.name = "CompetingEvents")
        comprisk[,Time := sapply(strsplit(as.character(Time),"_"),"[",2)]
        X = comprisk[X,on = "Time"]
        X[Time == 0,CompetingEvents := 0]
        X[,NumberAtrisk := NumberAtrisk-cumsum(CompetingEvents)]
    }
    vars = c("Time","NumberAtrisk","Treatment","Outcome")
    if(has_competing){vars = c(vars,"CompetingEvents")}
    if(length(x$Cnodes)>0){vars = c(vars,"Censored")}
    setcolorder(X,vars)
    print(X)
    invisible(X)
}

######################################################################
### print.prepare_Ltmle.R ends here
