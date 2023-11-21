### simulate_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Sep 12 2023 (14:24) 
## Version: 
## Last-Updated: Oct 16 2023 (16:00) 
##           By: Thomas Alexander Gerds
##     Update #: 19
#----------------------------------------------------------------------
## 
### Commentary: 
##
## treatment and covariate effects are given on the log-odds ratio scale
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
simulate_data <- function(n,
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
                            "Y_2" = c(0,0,0,0,0.0,0))){
  # 
  # NOTE: A_0 = _1-B_0 hence we do not need to generate B_0
  # 
  m <- lava::lvm(~Y_2+Y_1+Dead_1+Censored_1+Censored_2+A_1+A_0+B_1+L_1+L_0+W)
  lava::distribution(m,~Y_2+Y_1+Dead_1+Censored_1+Censored_2+A_1+A_0+B_1+L_1+L_0+W) <- lava::binomial.lvm()
  # in absence of other variables the intercept
  # is on the logit scale such that if we specify
  # an intercept of logit(0.8) then the risk = P(Y=1) is 0.8
  lava::intercept(m,~Y_1) <- lava::logit(outcome_prevalence[[1]])
  lava::intercept(m,~Y_2) <- lava::logit(outcome_prevalence[[2]])
  lava::intercept(m,~Censored_1) <- lava::logit(censoring_probability[[1]])
  lava::intercept(m,~Censored_2) <- lava::logit(censoring_probability[[1]])
  lava::intercept(m,~Dead_1) <- lava::logit(death_prevalence[[1]])
  lava::regression(m,L_1~W+L_0+A_0) <- effects[["Y_1"]]
  lava::regression(m,Y_1~W+L_0+A_0) <- effects[["Y_1"]]
  lava::regression(m,Dead_1~W+L_0+A_0) <- effects[["Dead_1"]]
  lava::regression(m,Censored_1~W+L_0+A_0) <- effects[["Censored_1"]]
  lava::regression(m,Censored_2~W+L_0+A_0) <- effects[["Censored_2"]]
  # dummy variable to ensure that if death but no outcome at time 1, then no outcome at time 2
  transform(m,dummy~Y_1+Dead_1) <- function(x){(1-x[1])*x[2]}
  lava::regression(m,Y_2~W+L_0+A_0+L_1+A_1+B_1+Y_1+dummy) <- c(effects[["Y_2"]], 1e+50, -1e+50)
  if (regimen){
    # set treatment A=1 og B=0
    lava::distribution(m,~A_0) <- lava::constant.lvm(1)
    lava::distribution(m,~A_1) <- lava::constant.lvm(1)
    lava::distribution(m,~B_1) <- lava::constant.lvm(0)
    # set censoring to uncensored
    lava::distribution(m,~Censored_1) <- lava::constant.lvm(0)
    lava::distribution(m,~Censored_2) <- lava::constant.lvm(0)
  }else{# observe treatments
    lava::regression(m,A_1~W+L_0+A_0) <- effects[["A_1"]]
    lava::regression(m,B_1~W+L_0+A_0) <- effects[["B_1"]]
  }
  d = lava::sim(m,n)
  setDT(d)
  d[,dummy:=NULL] # remove dummy variable from data
  d[,pnr:=1:.N]
  d
}



######################################################################
### simulate_data.R ends here
