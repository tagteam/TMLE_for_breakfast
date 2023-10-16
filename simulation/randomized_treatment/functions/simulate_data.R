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
                          outcome_prevalence = c(0.1,0.2),
                          death_prevalence = c(0.1),
                          censoring_probability = c(0.2),
                          effects = list(
                              # effects are W,L0,A0
                              "A1" = c(0,0,0),
                              "B1" = c(0,0,0),
                              "L1" = c(0,0,0),
                              "Y1" = c(0,0,0),
                              "C1" = c(0,0,0),
                              "D1" = c(0,0,0),
                              # effects are W,L0,A0,L1,A1,B1
                              "Y2" = c(0,0,0,0,0,0))){
    # 
    # NOTE: A0 = 1-B0 hence we do not need to generate B0
    # 
    m <- lava::lvm(~Y2+Y1+D1+C1+A1+A0+B1+L1+L0+W)
    lava::distribution(m,~Y2+Y1+D1+C1+A1+A0+B1+L1+L0+W) <- lava::binomial.lvm()
    # in absence of other variables the intercept
    # is on the logit scale such that if we specify
    # an intercept of logit(0.8) then the risk = P(Y=1) is 0.8
    lava::intercept(m,~Y1) <- lava::logit(outcome_prevalence[[1]])
    lava::intercept(m,~Y2) <- lava::logit(outcome_prevalence[[2]])
    lava::intercept(m,~C1) <- lava::logit(censoring_probability[[1]])
    lava::intercept(m,~D1) <- lava::logit(death_prevalence[[1]])
    lava::regression(m,L1~W+L0+A0) <- effects[["Y1"]]
    lava::regression(m,Y1~W+L0+A0) <- effects[["Y1"]]
    lava::regression(m,D1~W+L0+A0) <- effects[["D1"]]
    lava::regression(m,C1~W+L0+A0) <- effects[["C1"]]
    lava::regression(m,Y2~W+L0+A0+L1+A1+B1) <- effects[["Y2"]]
    lava::regression(m,A1~W+L0+A0) <- effects[["A1"]]
    lava::regression(m,B1~W+L0+A0) <- effects[["B1"]]
    d = lava::sim(m,n)
    setDT(d)
    d
}


######################################################################
### simulate_data.R ends here
