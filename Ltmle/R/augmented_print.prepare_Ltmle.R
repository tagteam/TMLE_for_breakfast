### print.prepare_Ltmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 19 2023 (15:57) 
## Version: 
## Last-Updated: May 22 2023 (09:25) 
##           By: Thomas Alexander Gerds
##     Update #: 61
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
    Outcome = c(0,sapply(x$Ynodes,function(y){sum(x$data[[y]],na.rm = TRUE)}))
    if (length(x$Cnodes)>0){
        Cens = c(0,sapply(x$Cnodes,function(c){sum(x$data[[c]] == "censored",na.rm = TRUE)}))
        Cens[length(Time)] = NA
    }
    if (length(x$info$Dnodes)>0){
        CR = c(0,sapply(x$info$Dnodes,function(d){sum(x$data[[d]],na.rm = TRUE)}),NA)
    }
    N.G = N.Q = rep(NROW(x$data),length(Time))
    if (length(Time)>1){
        N.G <- N.G-c(0,sapply(2:(length(Time) -1),function(j){
            sum((x$data[[x$Ynodes[j-1]]] == 1
                | (x$data[[x$Cnodes[j-1]]] == "censored")|is.na(x$data[[x$Cnodes[j-1]]])
                | (x$data[[x$info$Dnodes[j-1]]] == 1)),na.rm = TRUE)
        }),NA)
        if (x$info$order_YC){
            N.Q <- N.Q-c(0,sapply(2:(length(Time)-1),function(j){
                sum(((x$data[[x$Ynodes[j-1]]] == 1)
                    | (x$data[[x$Cnodes[j-1]]] == "censored")|is.na(x$data[[x$Cnodes[j-1]]])
                    | (x$data[[x$info$Dnodes[j-1]]] == 1)),na.rm = TRUE)
            }),NA)
        }else{
            N.Q <- N.Q-c(sum(x$data[[x$Cnodes[1]]] == "censored",na.rm = TRUE),sapply(2:(length(Time)-1),function(j){
                sum(((x$data[[x$Cnodes[j]]] == "censored")|is.na(x$data[[x$Cnodes[j]]])
                    | (x$data[[x$Ynodes[j-1]]] == 1) 
                    | (x$data[[x$info$Dnodes[j-1]]] == 1)),na.rm = TRUE)
            }),NA)
        }
        N.Q[length(Time)] = N.Q[length(Time)-1]-Outcome[length(Time)]
    }
    X = data.table(Time = Time,
                   NumberAtrisk.G = N.G,
                   NumberAtrisk.Q = N.Q,
                   Treat = Treat,
                   Outcome = Outcome)
    if (length(x$info$Dnodes)>0){X[,CompetingEvents := CR]}
    if (length(x$Cnodes)>0){X[,Censored := Cens]}
    print(X[])
    invisible(X[])
}

######################################################################
### print.prepare_Ltmle.R ends here
