### estimate_target_parameter.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Sep 21 2023 (15:34) 
## Version: 

## Last-Updated: Oct 16 2023 (16:02) 
##           By: Thomas Alexander Gerds
##     Update #: 5
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

estimate_target_parameter <- function(n,
                                      outcome_prevalence = c(0.1,0.1),
                                      death_prevalence = c(0.1),
                                      censoring_probability = c(0.2),
                                      regimen = FALSE,
                                      effects = list(
                                        # effects are W,L_0,A_0
                                        "A_1" = c(0,0,0),
                                        "B_1" = c(0,0,0),
                                        "L_1" = c(0,0,0),
                                        "Y_1" = c(0,0,0),
                                        "Censored_1" = c(0,0,0),
                                        "Censored_2" = c(0,0,0),
                                        "Dead_1" = c(0,0,0),
                                        # effects are W,L_0,A_0,L_1,A_1,B_1
                                        "Y_2" = c(0,0,0,0,0.0,0)),
                                      B_0 = FALSE,
                                      combine = FALSE,
                                      gbounds = c(0.01,1)){
  d = simulate_data(n = n,
                    outcome_prevalence = outcome_prevalence,
                    death_prevalence = death_prevalence,
                    censoring_probability = censoring_probability,
                    regimen = FALSE,
                    effects = effects)
  if(B_0){
    d[,B_0:=1-A_0]
    regimen_data = list("A" = d[,.(pnr,A_0,B_0,A_1,B_1)])
  }
  if(combine){
    d[,A_1:=A_1*(1-B_1)]
    regimen_data = list("A" = d[,.(pnr,A_0,A_1)])
  }
  regimen_data = list("A" = d[,.(pnr,A_0,A_1,B_1)])

  d[,{
    fit = run_ltmle(name_outcome = "Y",
                    time_horizon = 2,
                    regimen_data = regimen_data,
                    outcome_data = d[,.(pnr,Dead_1,Y_1,Censored_1,Y_2,Censored_2)],
                    baseline_data = d[,.(pnr,W)],
                    timevar_data = d[,.(pnr,L_0,L_1)],
                    censor_others = !combine, verbose = TRUE,
                    gbounds = gbounds)
    
    sum <- summary(fit[["time_horizon_2"]][["A"]]$Ltmle_fit)
    .(Estimate = sum[Target_parameter == "Risk(A=1)"]$estimate,
      SD = sum[Target_parameter == "Risk(A=1)"]$std.err,
      lower = sum[Target_parameter == "Risk(A=1)"]$lower,
      upper = sum[Target_parameter == "Risk(A=1)"]$upper)
  }]

}



######################################################################
### estimate_target_parameter.R ends here
