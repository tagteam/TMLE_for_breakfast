### estimate_target_parameter.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Sep 21 2023 (15:34) 
## Version: 
## Last-Updated: Sep 28 2023 (08:55) 
##           By: Thomas Alexander Gerds
##     Update #: 4
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
estimate_target_parameter <- function(){
    d = simulate_data(n = 200,treatment_effect = 0.1,covariate_effect = c(0.7,0))
    d[,{
        fit.unadjusted <- Ltmle(data = d[,.(W,A,Y)],
                                time_horizon = 1,
                                estimate.time = FALSE,
                                gbounds = c(0,1),
                                Lnodes = "W",
                                survivalOutcome = FALSE,
                                Qform = c("Y" = "Q.kplus1 ~ W + A"),
                                gform = "A~1",
                                Anodes = "A",
                                Ynodes = "Y",
                                verbose = FALSE,
                                abar = list(0,1))
        fit.adjusted <- Ltmle(data = d[,.(W,A,Y)],
                              time_horizon = 1,
                              estimate.time = FALSE,
                              gbounds = c(0,1),
                              Lnodes = "W",
                              survivalOutcome = FALSE,
                              Qform = c("Y" = "Q.kplus1 ~ W + A"),
                              gform = "A~W",
                              Anodes = "A",
                              Ynodes = "Y",
                              verbose = FALSE,
                              abar = list(0,1))
        sum.unadjusted <- summary.Ltmle(fit.unadjusted)
        sum.adjusted <- summary.Ltmle(fit.adjusted)
        .(Estimate = c("Hypothetical","Naive","Propensity unadjusted","Propensity adjusted"),
          Risk_treated = c(mean(Y_treated),mean(Y[A == 1]),sum.unadjusted[Target_parameter == "Mean(A=1)"]$estimate,sum.adjusted[Target_parameter == "Mean(A=1)"]$estimate),
          Risk_untreated = c(mean(Y_untreated),mean(Y[A == 0]),sum.unadjusted[Target_parameter == "Mean(A=0)"]$estimate,sum.adjusted[Target_parameter == "Mean(A=0)"]$estimate),
          ATE = c(mean(Y_treated)-mean(Y_untreated),
                  mean(Y[A == 1])-mean(Y[A == 0]),
                  sum.unadjusted[Target_parameter == "ATE"]$estimate,
                  sum.adjusted[Target_parameter == "ATE"]$estimate))
    }]
}


######################################################################
### estimate_target_parameter.R ends here
