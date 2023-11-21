### estimate_target_parameter.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Sep 21 2023 (15:34) 
## Version: 
## Last-Updated: Oct 11 (13:30)
##           By: Emilie Wessel
##     Update #: 4
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


# d = simulate_data(n = n,treatment_effect = treatment_effect,covariate_effect = covariate_effect)
# d[,{
#   fit.unadjusted <- Ltmle(data = d[,.(W,A,Y)],
#                           time_horizon = 1,
#                           estimate.time = FALSE,
#                           gbounds = c(0,1),
#                           Lnodes = "W",
#                           survivalOutcome = FALSE,
#                           Qform = c("Y" = "Q.kplus1 ~ W + A"),
#                           gform = "A~1",
#                           Anodes = "A",
#                           Ynodes = "Y",
#                           verbose = FALSE,
#                           abar = list(1,0))
#   fit.adjusted <- Ltmle(data = d[,.(W,A,Y)],
#                         time_horizon = 1,
#                         estimate.time = FALSE,
#                         gbounds = c(0,1),
#                         Lnodes = "W",
#                         survivalOutcome = FALSE,
#                         Qform = c("Y" = "Q.kplus1 ~ W + A"),
#                         gform = "A~W",
#                         Anodes = "A",
#                         Ynodes = "Y",
#                         verbose = FALSE,
#                         abar = list(1,0))
#   sum.unadjusted <- summary.Ltmle(fit.unadjusted)
#   sum.adjusted <- summary.Ltmle(fit.adjusted)
#   .(Estimate = c("Hypothetical","Naive","Propensity unadjusted","Propensity adjusted"),
#     Risk_treated = c(mean(Y_treated),mean(Y[A == 1]),sum.unadjusted[Target_parameter == "Mean(A=1)"]$estimate,sum.adjusted[Target_parameter == "Mean(A=1)"]$estimate),
#     Risk_untreated = c(mean(Y_untreated),mean(Y[A == 0]),sum.unadjusted[Target_parameter == "Mean(A=0)"]$estimate,sum.adjusted[Target_parameter == "Mean(A=0)"]$estimate),
#     ATE = c(mean(Y_treated)-mean(Y_untreated),
#             mean(Y[A == 1])-mean(Y[A == 0]),
#             sum.unadjusted[Target_parameter == "ATE"]$estimate,
#             sum.adjusted[Target_parameter == "ATE"]$estimate),
#     SE_Risk_treated = c(sd(Y_treated)/sqrt(n),sd(Y[A == 1])/sqrt(n),
#                         sum.unadjusted[Target_parameter == "Mean(A=1)"]$std.err,
#                         sum.adjusted[Target_parameter == "Mean(A=1)"]$std.err),
#     lower_Risk_treated = c(mean(Y_treated) - sd(Y_treated)/sqrt(n)*qnorm(.975),
#                            mean(Y[A == 1]) - sd(Y[A == 1])/sqrt(n)*qnorm(.975),
#                            sum.unadjusted[Target_parameter == "Mean(A=1)"]$lower,
#                            sum.adjusted[Target_parameter == "Mean(A=1)"]$lower),
#     upper_Risk_treated = c(mean(Y_treated) + sd(Y_treated)/sqrt(n)*qnorm(.975),
#                            mean(Y[A == 1]) + sd(Y[A == 1])/sqrt(n)*qnorm(.975),
#                            sum.unadjusted[Target_parameter == "Mean(A=1)"]$upper,
#                            sum.adjusted[Target_parameter == "Mean(A=1)"]$upper),
#     SE_Risk_untreated = c(sd(Y_untreated)/sqrt(n),sd(Y[A == 0])/sqrt(n),
#                           sum.unadjusted[Target_parameter == "Mean(A=0)"]$std.err,
#                           sum.adjusted[Target_parameter == "Mean(A=0)"]$std.err),
#     lower_Risk_untreated = c(mean(Y_untreated) - sd(Y_untreated)/sqrt(n)*qnorm(.975),
#                              mean(Y[A == 0]) - sd(Y[A == 0])/sqrt(n)*qnorm(.975),
#                              sum.unadjusted[Target_parameter == "Mean(A=0)"]$lower,
#                              sum.adjusted[Target_parameter == "Mean(A=0)"]$lower),
#     upper_Risk_untreated = c(mean(Y_untreated) + sd(Y_untreated)/sqrt(n)*qnorm(.975),
#                              mean(Y[A == 0]) + sd(Y[A == 0])/sqrt(n)*qnorm(.975),
#                              sum.unadjusted[Target_parameter == "Mean(A=0)"]$upper,
#                              sum.adjusted[Target_parameter == "Mean(A=0)"]$upper),
#     SE_ATE = c(sd(Y_treated - Y_untreated)/sqrt(n),sd(Y[A == 1] - Y[A == 0])/sqrt(n),
#                sum.unadjusted[Target_parameter == "ATE"]$std.err,
#                sum.adjusted[Target_parameter == "ATE"]$std.err),
#     lower_ATE = c(mean(Y_treated)-mean(Y_untreated) - sd(Y_treated - Y_untreated)/sqrt(n)*qnorm(.975),
#                   mean(Y[A == 1])-mean(Y[A == 0]) - sd(Y[A == 1] - Y[A == 0])/sqrt(n)*qnorm(.975),
#                   sum.unadjusted[Target_parameter == "ATE"]$lower,
#                   sum.adjusted[Target_parameter == "ATE"]$lower),
#     upper_ATE = c(mean(Y_treated)-mean(Y_untreated) + sd(Y_treated - Y_untreated)/sqrt(n)*qnorm(.975),
#                   mean(Y[A == 1])-mean(Y[A == 0]) + sd(Y[A == 1] - Y[A == 0])/sqrt(n)*qnorm(.975),
#                   sum.unadjusted[Target_parameter == "ATE"]$upper,
#                   sum.adjusted[Target_parameter == "ATE"]$upper))
# }]


######################################################################
### estimate_target_parameter.R ends here
