### print.prepare_Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 19 2023 (15:57) 
## Version: 
## Last-Updated: May 20 2023 (14:55) 
##           By: Thomas Alexander Gerds
##     Update #: 48
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
    Time = x$info$time_grid
    Treat = c(sapply(x$Anodes,function(a){sum(x$data[[a]],na.rm = TRUE)}),NA)
    Out = c(0,sapply(x$Ynodes,function(y){sum(x$data[[y]],na.rm = TRUE)}))
    N = NROW(x$data) -cumsum(Out)
    if (length(x$Cnodes)>0){
        if (x$info$order_YC)
            Cens = c(0,sapply(x$Cnodes,function(c){sum(x$data[[c]] == "censored",na.rm = TRUE)}),NA)
        else
            Cens = c(0,sapply(x$Cnodes,function(c){sum(x$data[[c]] == "censored",na.rm = TRUE)}))
        N = N-cumsum(Cens)
    }
    if (length(x$info$Dnodes)>0){
        CR = c(0,sapply(x$info$Dnodes,function(d){sum(x$data[[d]],na.rm = TRUE)}),NA)
        N = N-cumsum(CR)
    }
    X = data.table(Time = Time,NumberAtrisk = N,Treat = Treat,Outcome = Out)
    if (length(x$info$Dnodes)>0){X[,CompetingEvents := CR]}
    if (length(x$Cnodes)>0){X[,Censored := Cens]}
    print(X[])
    invisible(X[])
}

######################################################################
### print.prepare_Ltmle.R ends here
