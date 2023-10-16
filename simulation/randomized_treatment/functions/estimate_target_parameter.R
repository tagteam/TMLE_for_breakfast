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
estimate_target_parameter <- function(){
    d = simulate_data(n,
                      outcome_prevalence = c(0.1,0.2),
                      death_prevalence = c(0.1),
                      censoring_probability = c(0.2),
                      effects = list("A1" = c(0,0,0),
                                     "B1" = c(0,0,0),
                                     "L1" = c(0,0,0),
                                     "Y1" = c(0,0,0),
                                     "C1" = c(0,0,0),
                                     "D1" = c(0,0,0),
                                     "Y2" = c(0,0,0,0,0,0)))
    
}


######################################################################
### estimate_target_parameter.R ends here
