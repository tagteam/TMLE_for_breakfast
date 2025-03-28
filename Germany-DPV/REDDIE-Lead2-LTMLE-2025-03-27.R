#############################################################################
###                           REDDIE - LTMLE                              ###
###                 Metformin-GLP vs. Metformin-DPP4                      ###
#############################################################################

library(heaven)
library(data.table)

library(lava)
library(foreach)

library(matrixStats)
library(SuperLearner)

library(rtmle)



options(rstudio.help.showDataPreview = FALSE)

try(setwd("\\\\buster.zibmt.uni-ulm.de\\schmid\\Projekte\\REDDIE_WP4\\TMLE_for_breakfast-main\\Ltmle\\"),silent = TRUE)


## copy of functions from CRAN package ltmle
#ff <- sapply(list.files(path = "./R/",pattern = "R$",full.names = TRUE),source)
## our own augmentation files
#ff <- sapply(list.files(path = "./augmentation/",pattern = "R$",full.names = TRUE),source)


### read datasets
SAS_pat <- read.table("\\\\keaton.zibmt.uni-ulm.de\\SAS-DPV-Pseudo\\a-dpv-anonym\\R-Arbeitsdaten\\REDDIE\\LEAD2_pat.txt", sep="\t", header=TRUE, dec=" ")

SAS_vis <- read.table("\\\\keaton.zibmt.uni-ulm.de\\SAS-DPV-Pseudo\\a-dpv-anonym\\R-Arbeitsdaten\\REDDIE\\LEAD2_visits.txt", sep="\t", header=TRUE, dec=" ")


# convert SAS date-format 
SAS_pat$baseline_dat   <- as.Date(SAS_pat$baseline_dat, origin = "1960-01-01")
SAS_pat$tod_dat        <- as.Date(SAS_pat$TOD_DAT,      origin = "1960-01-01")
SAS_pat$censor_dat     <- as.Date(SAS_pat$censor_dat,   origin = "1960-01-01")
SAS_pat$hba75_dat      <- as.Date(SAS_pat$hba75_dat,    origin = "1960-01-01")

SAS_vis$treatment_start <- as.Date(SAS_vis$treatment_start, origin = "1960-01-01")
SAS_vis$treatment_end   <- as.Date(SAS_vis$treatment_end,  origin = "1960-01-01")

# convert as double
SAS_pat$baseline_age   <- as.double(SAS_pat$baseline_age)
SAS_pat$baseline_dur   <- as.double(SAS_pat$baseline_dur)

SAS_vis$value          <- as.double(SAS_vis$value)



# split the dataset to baseline, outcome and treatment

baseline_data <- SAS_pat[, c("id", "baseline_dat", "sex", "baseline_age", "baseline_dur")]
baseline_data <- as.data.table(baseline_data)

base <- baseline_data[,.(id, baseline_dat)]
base[,':='(inn=baseline_dat,out=baseline_dat+4*180)]


outcome_data <- SAS_pat[, c("id", "tod_dat", "censor_dat", "hba75_dat")]
setnames(outcome_data, c("id", "compete", "censor", "outcome"))
outcome_data <- as.data.table(outcome_data)


treatment_data <- SAS_vis[, c("id", "treatment_start", "treatment_end", "treatment", "value")]
treatment_data <- as.data.table(treatment_data)


## longsplit using the heaven-package

longSplit <- splitTwo(indat=base,
                      splitdat=outcome_data,
                      invars=c('id', 'inn', 'out'),
                      splitvars = c('id', 'outcome', 'censor', 'compete'))

longSplit2 <- splitFromTo(indat=longSplit,
                          splitdat=treatment_data,
                          invars=c('id','inn','out'),
                          splitvars = c('id','treatment_start','treatment_end', 'value', 'treatment'))

longSplit3 <- splitSeq(indat=longSplit2,
                       invars=c('id','inn','out'),
                       varname = 'baseline_dat', # intervals since inclusion in study
                       splitvector= c(-1, 180, 360, 540, 720),
                       format = "vector",
                       value="period")


## aggregate for period

setkeyv(longSplit3,c("id", "period", "inn"))

# Max value of outcomes in each period
longSplit3[,':='(outcome=max(outcome),censor=max(censor),compete=max(compete)), by=c("id", "period")] 

setkeyv(longSplit3, c("id", "inn"))

# Choose first record for each ID/period
aggrSplit <- longSplit3[,.SD[1],by=c("id","period")]
aggrSplit[,GLPA:=as.numeric(GLPA)]
aggrSplit[,DPP4:=as.numeric(DPP4)]


# shift covariates one period back
aggrSplit_cov <- aggrSplit[,.(id, period, DPP4, GLPA)]                 # time dependant variables
aggrSplit_cov[,period:=period-1]
aggrSplit_out <- aggrSplit[,.(id, period, compete, censor, outcome)]   # outcomes


# versions for rtmle
datalist <- list(
  timevar_data=aggrSplit_cov,
  outcome=aggrSplit_out[outcome==1,.(id,period)],
  compete=aggrSplit_out[compete==1,.(id,period)],
  censor=aggrSplit_out[censor==1,.(id,period)]
)




