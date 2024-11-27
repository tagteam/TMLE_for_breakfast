library(heaven) # devtools::install_github("tagteam/heaven")
library(data.table)
#library(ltmle)
library(rtmle) # devtools::install_github("tagteam/rtmle")
library(targets)
library(ggplot2)
# source functions
#tar_source("/home/ctp/github/TMLE_for_breakfast/Ltmle/R/")
try(setwd('~/github/TMLE_for_breakfast/Example_rtmle/'),silent = TRUE)
try(setwd("~/research/Methods/TMLE_for_breakfast//"),silent = TRUE)
baseline_data <- readRDS("Example/baseline_data.rds")
treatment_data <- readRDS("Example/treatment_data.rds")
head(baseline_data)
base <- baseline_data[,.(ID,Trial_Start)]
base[,':='(inn=Trial_Start,out=Trial_Start+4*365)]
splitguideTwo <- baseline_data[,.(ID,Trial_Start,Event_Type,Time)]
splitguideTwo[,eventTime:=Trial_Start+Time]
# use the multilevel event variable to create separate variable/time for censoring, outcome and competing risk
splitguideTwo[Event_Type==0,censor:=eventTime]
splitguideTwo[Event_Type==1,outcome:=eventTime]
splitguideTwo[Event_Type==2,compete:=eventTime]
longSplit <- splitTwo(indat=base,
                      splitdat=splitguideTwo,
                      invars=c('ID','inn','out'),
                      splitvars = c('ID','censor','outcome','compete'))
splitguideTime <- copy(treatment_data)
splitguideTime[,':='(treatment="treatment",value=1)]
longSplit <- splitFromTo(indat=longSplit,
                         splitdat=splitguideTime,
                         invars=c('ID','inn','out'),
                         splitvars = c('ID','Treatment_Start','Treatment_End','value','treatment'))
longSplit <- splitSeq(indat=longSplit,
                      invars=c('ID','inn','out'),
                      varname = 'Trial_Start', # intervals since inclusion in study
                      splitvector= seq(-1,5*365-1,365), # just two periods for the example
                      format = "vector",
                      value="period")
setkeyv(longSplit,c("ID","period","inn"))
# Max value of outcomes in each period
longSplit[,':='(outcome=max(outcome),censor=max(censor),compete=max(compete)),
          by=c("ID","period")] 
setkeyv(longSplit,c("ID","inn"))
# Choose first record for each ID/period
aggrSplit <- longSplit[,.SD[1],by=c("ID","period")]
aggrSplit[,treatment:=as.numeric(treatment)]
# for rtmle:
aggrSplit[,period:=as.Date(period,origin="2024-01-01")]
# Make relation between censor, outcome and death possible - not relevant for REAL data

# shift outcomes one period forward
aggrSplit_cov <- aggrSplit[,.(ID,period,treatment)] # all time dependent variables, here just "treatment"
aggrSplit_cov[,period:=period-1]
aggrSplit_out <- aggrSplit[,.(ID,period,outcome,censor,compete)] # outcomes
# versions for rtmle
time_var_data <- list(
    time_var_cov=aggrSplit_cov,
    outcome_data=aggrSplit_out[outcome==1,.(ID,period)],
    competing_data=aggrSplit_out[compete==1,.(ID,period)],
    censor_data=aggrSplit_out[censor==1,.(ID,period)]
)
x <- rtmle_init(intervals=5,
                name_id='ID',
                name_time='period',
                name_outcome='outcome',
                name_competing='compete',
                name_censoring='censor',
                treatment_levels=c('0','1'),
                censored_levels=c('1','0'),
                censored_label="1")
add_long_data(x) <- time_var_data
add_baseline_data(x) <- baseline_data
x
