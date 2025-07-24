### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: May  1 2025 (06:50) 
## Version: 
## Last-Updated: Jul 21 2025 (11:50) 
##           By: Thomas Alexander Gerds
##     Update #: 16
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
tar_option_set(packages = c("tidyverse","data.table", "riskRegression"))
# Manal (adapt the path to your computer):
#try(setwd("c:/git/TMLE_for_breakfast/imperial"),silent = TRUE)
rtmle_code <- "C:/git/rtmle-main/R/"
if (file.exists(rtmle_code)) tar_source(rtmle_code)
# Thomas:
try(setwd("~/research/Methods/TMLE_for_breakfast/imperial"),silent = TRUE)
try(library(rtmle),silent = TRUE)

tar_source("functions")

# pipeline
list(tar_target(dummy_data,{
   # get_dummy_data(filename = "data/01-Dummy_data_to_share_DATES.xlsx")
    get_dummy_data(filename = "data/UPDATEDATA.xlsx")
}),tar_target(test_rtmle,{
    get_test_rtmle(dummy_data = dummy_data)
})
)


######################################################################
### _targets.R ends here
