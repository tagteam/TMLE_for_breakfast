set.seed(42)
library(data.table)
library(prodlim)
setwd('~/github/TMLE_for_breakfast/Example/')
# Number of patients
n_patients <- 1000
# Assigning treatment (0/1)
treatment_assignment <- c(rep(1,n_patients/2),rep(0,n_patients/2))
# Generating ages
age <- sample(c('<60', '60-70', '>70'), size = n_patients, replace = TRUE)
# Generating time from start of trial
time <- rexp(n_patients, rate = 1) # CTP changed from 2/5
# Generating persistent treatment periods for half of the treated patients
persistent_treatment_periods <- 
  data.table(ID=1:(n_patients/4),Treatment_Start=0, Treatment_End=time[1:(n_patients/4)])
non_persistent_treatment_periods <- 
  data.table(ID=(1+n_patients/4):(n_patients/2),Treatment_Start=0, Treatment_End=time[(1+n_patients/4):(n_patients/2)])  
treatment_periods <- rbind(persistent_treatment_periods,non_persistent_treatment_periods)

# Generating event type (censoring=0, event=1, death=2)
event_type1 <- sample(c(0, 1, 2), size = n_patients/4, prob = c(0.2, 0.2, 0.1), replace = TRUE)
event_type2 <- sample(c(0, 1, 2), size = n_patients/4, prob = c(0.2, 0.4, 0.1), replace = TRUE)
event_type3 <- sample(c(0, 1, 2), size = n_patients/2, prob = c(0.2, 0.7, 0.1), replace = TRUE)
event_type <- c(event_type1,event_type2,event_type3)
# Creating data
time <- time*365
baseline_data <- data.table(ID = 1:n_patients,
                            Age = age,
                            Time = time,
                            Event_Type = event_type,
                            Study_Start=0,
                            Treatment=c(rep(1,n_patients/2),rep(0,n_patients/2)))
# Creating data for treatment periods dataset
treatment_data <- data.table(ID=1:(n_patients/2),treatment_periods[,ID:=NULL])
treatment_data[Treatment_End>5,Treatment_End:=5]

baseline_data[,Trial_Start:=as.Date("2010-01-01")]
treatment_data[,Treatment_Start:=as.Date("2010-01-01")]
treatment_data[,Treatment_Start:=as.Date(Treatment_Start,origin="1970-01-01")]
treatment_data[,Treatment_End:=as.Date("2010-01-01")+365*Treatment_End]

fit <- prodlim(Hist(Time,Event_Type)~Treatment,data=baseline_data)
plot(fit)
saveRDS(baseline_data,file="baseline_data.rds")
saveRDS(treatment_data,file="treatment_data.rds")


