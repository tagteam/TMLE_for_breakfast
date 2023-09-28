### simulate_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Sep 12 2023 (14:24) 
## Version: 
## Last-Updated: Sep 13 2023 (11:16) 
##           By: Thomas Alexander Gerds
##     Update #: 12
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
                          outcome_prevalence = 0.2,
                          treatment_effect, 
                          covariate_effect = c(0,0)){
    m <- lava::lvm(~Y_treated+Y_untreated+A+W)
    lava::distribution(m,~Y_treated+Y_untreated+A+W) <- lava::binomial.lvm()
    # in absence of other variables the intercept
    # is on the logit scale such that if we specify
    # an intercept of logit(0.8) then the risk = P(Y=1) is 0.8
    lava::intercept(m,~Y_untreated) <- lava::logit(outcome_prevalence)
    # the treatment effect is specified as a risk difference
    if (treatment_effect>outcome_prevalence){stop("Treatment effect is larger than risk of the outcome.")}
    lava::intercept(m,~Y_treated) <- logit(outcome_prevalence-treatment_effect)
    lava::regression(m,Y_treated~W) <- covariate_effect[[1]]
    lava::regression(m,Y_untreated~W) <- covariate_effect[[1]]
    lava::regression(m,A~W) <- covariate_effect[[2]]
    lava::transform(m,Y~Y_treated+Y_untreated+A) = function(x){
        x[["Y_treated"]]*(x[["A"]] == 1)+ x[["Y_untreated"]]*(x[["A"]] == 0)
    }
    d = lava::sim(m,n)
    setDT(d)
    d
}


######################################################################
### simulate_data.R ends here
