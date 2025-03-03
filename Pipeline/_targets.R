#Insulin script
#by PDWY

setwd("Z:/Workdata/703740/Puriya/insulin")

# Load packages required to define the pipeline: ----
library(targets)
library(tarchetypes)
library(crew)

## tar_cues
cue=function(x){tar_cue(mode=x)}

import_mode <-cue("thorough")
manage_mode <- cue("thorough")
block_mode <- cue("never")
analys_mode <- cue("thorough")
my_mode <- cue("thorough")

para=TRUE
if (para){
  library(doParallel)
  cl=makeCluster(25)
  registerDoParallel(cl)
}
## Set target options: ----
tar_option_set(
  packages = c("data.table","Publish","heaven","ggplot2",
               "parallel","doParallel","haven","foreach","prodlim",
               "DiagrammeR","writexl","DiagrammeRsvg","rsvg","png","zoo"), 
  format = "rds", # default storage format
    controller=crew_controller_local(workers=30, garbage_collection = TRUE) 
)

# Load the R scripts with your custom functions:
tar_source("functions")
tar_source("z:/Workdata/703740/R-packages/abgespeckt_Ltmle/R/")


list(
  ## Lists (all under the lists.r file) ----
  tar_target(diamedlist,diamed_list(),cue=import_mode),
  tar_target(medlist,baselinemed_list(),cue=import_mode),
  tar_target(comorblist,comorb_list(),cue=import_mode),
  tar_target(procedureslist,procedures_list()),
  tar_target(originCountryList, origincountry_LIST()),
  
  
################################################################################
#
# DK specific data import of raw data 
#
################################################################################
  

## Identify cohort of interest + define end-dates of treatment ----
  
  # Import medication only A10 (used to generate target populaton and then pnrlist)
  tar_target(lmdb_dia, fx.get.lmdb.dia(diamedlist=diamedlist),cue=import_mode),
  
  # Identify all dpp4 and sglt2 users, define end date, and define index date and arms
  tar_target(dt_deg_glg, get_deg_glg(lmdb_dia=lmdb_dia,length_prescrip=180),cue=import_mode),
  
  # Make dateset of all PNRs to filter future large datasets on 
  tar_target(pnrlist,{return(unique(dt_deg_glg[,.(pnr)]))},cue=import_mode),
  
  # Dataset of only pnr and index date and specify study period of interest
  tar_target(dt_index, fx.index.population(dt_deg_glg=dt_deg_glg),cue=import_mode),
 
## Import large datesets ----
  
  # Import all medication 
  tar_target(lmdb, fx.get.lmdb(pnrlist=pnrlist),cue=import_mode),
  
  #Import death 
  tar_target(death, {importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/death/dod.sas7bdat",filter=pnrlist)},cue=import_mode),
  
  # Import DDV (dansk diabetes voksen register)
  tar_target(ddv_dia,{importSAS("X:/Data/Rawdata_Hurtig/703740/Eksterne data/dvdd_2023_01_31.sas7bdat",filter=pnrlist)},cue=import_mode),
  
  # All laboratory results
  tar_target(dt_lab, get_lab(pnrlist=pnrlist,
                           ddv_dia=ddv_dia, nobs=Inf,
                           hba.lower=0.19,hba.upper=121),cue=import_mode), #Indicator of outliers of HbA1c
  #Import LPR
  tar_target(LPR, fx.get.LPR(pnrlist=pnrlist),cue=manage_mode),

  # Procedures (to identify re-vascularzation) 
  tar_target(procedures,get_procedures(pnrlist=pnrlist)),
  
  # Sex and Age
  tar_target(sexage,{importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/Population/sexbirth.sas7bdat",filter=pnrlist)},cue=manage_mode),
  
  # Immi/emmi 
  tar_target(immiemi,{importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/Population/vnds2023.sas7bdat",filter=pnrlist)},cue=manage_mode),
  
  # Import info on municipality from BEF
  tar_target(kom, fx.get.kom(pnrlist=pnrlist),cue=manage_mode),
  
  #Import education
  tar_target(education, fx.get.education(pnrlist=pnrlist),cue=manage_mode),
  
  #import income
  tar_target(income, fx.get.income(pnrlist=pnrlist),cue=manage_mode),
  
################################################################################
#
# Data management of raw data and identification of study population  
#
################################################################################

  ## Make datasets with comorbidities (from index date), ----
  tar_target(dt_comorb, fx.get.comorb(data=dt_index,
                                      comorblist=comorblist,
                                      LPR=LPR,
                                      procedureslist=procedureslist,
                                      procedures=procedures,
                                      lmdb=lmdb),cue=manage_mode),

  # Wide dataset version: not necessary per se, but nice to have for descriptive purposes
  tar_target(dt_comorb_wide, fx.make.comorb.wide(dt_comorb=dt_comorb),cue=manage_mode),
  

  # Make datasets with baseline meds/covariates ----
  
  # Baseline demographics ----
  tar_target(dt_baseline, fx.get_baseline_covar(data=dt_index,
                                                income=income,
                                                education=education,
                                                sexage=sexage,
                                                kom=kom),cue=manage_mode),

  # Concomitant medical use looking 180 days back ----
  tar_target(dt_med,get_baseline_med(data=dt_index,
                                     lmdb=lmdb,
                                     medlist=medlist),cue=manage_mode),

  # Diabetes baseline medicine ----
  tar_target(dt_med_dia,get_baseline_med_dia(data=dt_index,
                                             lmdb=lmdb_dia,
                                             diamedlist=diamedlist),cue=manage_mode),

  # Diabetes med post baseline (list of datasets) ----
  tar_target(dt_med_dia_post,fx.post.med.dia(data=dt_index,
                                             lmdb=lmdb,
                                             diamedlist=diamedlist),cue=manage_mode),

  # Baseline comorbidities ----
  tar_target(dt_comorb_baseline,get_comorb_baseline(data=dt_index,
                                                    dt_comorb_wide=dt_comorb_wide,
                                                    nyears=10),cue=manage_mode), #nyears is an indicator of how long we are looking back for definitions of comorbidities 
  
  # Exclusions ----
  tar_target(exclusions_DEVOTE,exclusions.devoteFUN(dt_index=dt_index,
                                             immiemi=immiemi,
                                             LPR=LPR,
                                             lmdb=lmdb,
                                             dt_baseline=dt_baseline,
                                             dt_deg_glg=dt_deg_glg,
                                             dt_comorb_wide=dt_comorb_wide,
                                             dt_lab=dt_lab,
                                             sexage=sexage,
                                             maxyears=20, #number of years looking back for CVD disease
                                             hbalim=53),cue=manage_mode), #minimum hba1c for inclusion

## Endpoints / outcomes ----
# Outcomes list format: Composite primary and secondary outcomes not defined yet ----
tar_target(dt_outcomes,fx.get.outcomes(data=exclusions_DEVOTE,
                                       LPR=LPR,
                                       comorblist=comorblist,
                                       death=death,
                                       dt_comorb_wide=dt_comorb_wide,
                                       procedureslist=procedureslist,
                                       procedures=procedures),cue=manage_mode),

tar_target(regime_data_prep,fx.regime_prep(exclusions_DEVOTE=exclusions_DEVOTE,
                                 immiemi=immiemi,
                                 death=death,
                                 dt_deg_glg=dt_deg_glg,
                                 end_of_followup=as.Date("2022-12-31"),
                                 end_of_data=as.Date("2022-12-31")),cue=analys_mode),#End of data must be less than end of followup)

################################################################################
#
# Data preparation for LTMLE analysis using the above prepared datasets 
#
################################################################################


# Treatment data in wide format
# ----------------------------------------------------------------------------
# This object consists of a list of two datasets (data.tables) in wide format
# defining exposure to the two therapies of interest, respectively. 
# These contains of one row per all individuals included, and variables defining each interval. 
# The levels can either we 0, 1 or NA (after censoring or mortality)
# Exposure to the two the therapies is not mutually exclusive. 
tar_target(regime_data, {
  regime_data_prep[,drugclass:=factor(drugclass,levels=c("dpp4_inhib","glp1","insulin_aspart","glargine","degludec","metformin","other","sglt2_inhib","sulfonylurea"),
                                 labels=c("dpp4","glp1","ins_aspart","glargine","degludec","metformin","other","sglt2","sulfonylurea"))]
  fx.widen.regimens.LTMLEprep(regime_data_prep=regime_data_prep,
                              intervals=seq(0,10*6*30.45,6*30.45),
                              add_B_nodes=TRUE
  )} ,cue=analys_mode),


# Baseline data 
# ----------------------------------------------------------------------------
# The object is data.table with one row per individual 
# Composed for two purposes 
#  1) Baseline characteristics 
#  2) In combination with "names_baseline_covariates" to be used for LTMLE analysis 
tar_target(baseline_data, fx.baseline.all.LTMLEprep(dt_index=dt_index,
                                                                   dt_med=dt_med,
                                                                   dt_med_dia=dt_med_dia,
                                                                   dt_comorb_baseline=dt_comorb_baseline,
                                                                   regime_data_prep=regime_data_prep,
                                                                   dt_baseline=dt_baseline,
                                                                   dt_deg_glg=dt_deg_glg,
                                                                   dt_lab=dt_lab),
           cue=analys_mode),


# Timevarying data in wide format
# ------------------------------------------------------------------------------
# Wide dataset with one row per individual. 
# Consists of one id variable and then variables at each point, eg. metformin_0, metformin_1, and so on. 
# for alle timevarying variables. Can be both binary, categorical, or numeric. 
tar_target(timevar_data,fx.widen.covar.LTMLEprep(regime_data_prep=regime_data_prep,
                                                            intervals=seq(0,10*6*30.45,6*30.45),
                                                            dt_med_dia_post=dt_med_dia_post,
                                                            dt_comorb=dt_comorb,
                                                            exclusions_DEVOTE=exclusions_DEVOTE),cue=analys_mode),


# Outcome data in wide format
# ----------------------------------------------------------------------------
# Named list of datasets for each outome 
# Each dataset consists one row per individual. 
# Each dataset haas an id variable and
# variables for each timepoint 0-10 defining the occurance of 
# 1) The outcome in question 
# 2) Censoring 
# 3) Mortality 

# Primary outcomes: Prepare wide outcome data
tar_target(primary_outcomes,{
  # Include filter by pnrlist
  outcome_list=list(
    "cvddeath"="cvddeath",
    "mi"="mi",
    "stroke"="stroke",
    "resvarcular"="resvarcular",
    "heart.failure"="heart.failure",
    "uap"="uap",
    "mace"=c("mi","stroke","cvddeath"),
    "death"="death",
    "mace+death"=c("mi","stroke","death"),
    "mace+uap"=c("mi","stroke","cvddeath","uap"),
    "Hypogly"="Hypogly",
    "nonCVDdeath"="nonCVDdeath")
  # "tia" ="tia")
  olist=lapply(names(outcome_list),function(OUT){
    widen_outcome(outcome=outcome_list[OUT],
                  data=regime_data_prep,
                  intervals=seq(0,10*6*30.45,6*30.45),
                  como=dt_outcomes
    )
  })
  names(olist)=names(outcome_list)
  olist
},cue=analys_mode),


# Specify baseline only covariates
# ----------------------------------------------------------------------------
# This object is just a vector of names of variables 
# For those covariates that are both included as baseline and post-baseline variables
# we include only in the timevarying object, bc. they are then included at baseline at time 0 
tar_target(names_baseline_covariates,
           c("pnr","age","sex",
             "incomegroup", 
             "urbanization","edu", 
             "diabetes_duration_group",
             "hba_res",
             "mra_180","bb_180","rasi_180","loop_180","ccb_180",
             "thiazid_180","asa_180","statin_180"
           )
           ,cue=analys_mode),


################################################################################
#
# LTMLE analysis 
#
################################################################################

  #Primary analysis ----
  #Final idenfifier of population for analysis (could have been inluded longer up the stream)
    tar_target(core,{core=baseline_data[age<90]
    core=exclusions_DEVOTE[core,on="pnr"]
    core[,index.year:=year(index.date)]
    core=core[index.year>=2016&index.year<=2021]
    }),

PRIMARY_ANALYSES <- tar_map(values=expand.grid(
  #Define names of outcomes 
  OUT=c("cvddeath","mace","death","mi","stroke","heart.failure","uap","mace+uap","Hypogly","nonCVDdeath"), #
  #Define names of subgroup analyses
  SUB=c("all",
        "s_age_under65",
        "s_age_over65",
        "s_age_under75",
        "s_age_over75",
        "s_male",
        "s_female",
        "s_diadur_above10",
        "s_diadur_below10",
        "s_gfr_above90",
        "s_gfr_60-90",
        "s_gfr_bel60",
        "s_hba_under",
        "s_hba_over",
        "s_statins_yes",
        "s_statins_no",
        "s_insulin_yes",
        "s_insulin_no"
  ),
  # .. ad further sensitivity analyses if any
  stringsAsFactors=FALSE),
  tar_target(name=primary_analysis,
             command={
               # Define datasets to run the analysis on
               included_patients=switch(SUB,
                                        "all"=core[,.(pnr)],
                                        "s_age_under65" = core[age<65,.(pnr)],
                                        "s_age_over65"= core[age>=65,.(pnr)],
                                        "s_age_under75" = core[age<75,.(pnr)],
                                        "s_age_over75"= core[age>=75,.(pnr)],
                                        "s_male"=core[sex=="male",.(pnr)],
                                        "s_female"=core[sex=="female",.(pnr)],
                                        "s_diadur_above10" = core[diabetes_duration_group=="> 10",.(pnr)],
                                        "s_diadur_below10" = core[diabetes_duration_group!="> 10",.(pnr)],
                                        "s_gfr_above90" = core[egfr>=90,.(pnr)],
                                        "s_gfr_60-90" = core[egfr<90&egfr>=60,.(pnr)],
                                        "s_gfr_bel60" = core[egfr<60,.(pnr)],
                                        "s_hba_under" = core[hba_res<69,.(pnr)],
                                        "s_hba_over" = core[hba_res>=69,.(pnr)],
                                        "s_statins_yes" = core[statin_180==TRUE,.(pnr)],
                                        "s_statins_no" = core[statin_180==FALSE,.(pnr)],
                                        "s_insulin_yes" = core[insulin_aspart_180==TRUE,.(pnr)],
                                        "s_insulin_no" = core[insulin_aspart_180==FALSE,.(pnr)],

               )
               
               ## LTMLE analysis
               print(OUT)
               print(SUB)
               fit= run_ltmle(name_outcome=OUT,
                              time_horizon=1:8,
                              sub_set=included_patients,
                              test=FALSE,
                              regimen_data=regime_data,
                              outcome_data=primary_outcomes,
                              baseline_data=baseline_data[,names_baseline_covariates,with=FALSE],
                              timevar_data=timevar_data,
                              B_bootstrap_samples=25,
                              bootstrap_sample_size=ceiling(.632*NROW(included_patients)),
                              SL.library="glmnet",
                              censor_others=TRUE,
                              Markov=NULL,
                              verbose=FALSE,
                              SL.cvControl=list(selector="undersmooth",alpha=0.5),
                              gbounds=c(0,1)
                              )
               fit
             },cue=analys_mode)

  )

)