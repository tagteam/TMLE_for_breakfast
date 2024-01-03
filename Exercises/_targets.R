### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May 11 2023 (13:22) 
## Version: 
## Last-Updated: May 18 2023 (09:51) 
##           By: Thomas Alexander Gerds
##     Update #: 19
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
try(setwd("~/research/Methods/TMLE_for_breakfast/Exercises"))
library(targets)
library(data.table)
library(lava)
for (f in list.files("functions/",
                     pattern = "R$",
                     full.names = TRUE)){source(f)}
for (f in list.files("../Ltmle/R/",pattern = "R$",full.names = TRUE)){source(f)}
for (f in list.files("../Ltmle/augmentation/",pattern = "R$",full.names = TRUE)){source(f)}
list(
    tar_target(rdata,{
        simulate_rdata(n = 78000)
    })
)

######################################################################
### _targets.R ends here
