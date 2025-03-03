fx.get.outcomes <- function(data,LPR,comorblist,death,death_cause2,dt_comorb_wide,procedureslist,procedures) {
  
  # tar_load(pnrlist_EMPAREG)
  # data=pnrlist_EMPAREG
  # tar_load(comorblist)
  # tar_load(dt_comorb_wide)
  # tar_load(death)
  # tar_load(LPR)
  
  # dt_comorb=dt_comorb_wide
  dt_comorb_wide <- dt_comorb_wide[!is.na(pnr)] #One pnr is na. This is not the case in the final population.
  
  
  
  outlist <- list(comorblist$heart.failure,comorblist$mi,comorblist$stroke,procedureslist$resvarcular,comorblist$uap,comorblist$diab_retinopathy,comorblist$diab_nefropathy,comorblist$dialysis,comorblist$tia,comorblist$Hypogly)
  names(outlist) <- c("heart.failure","mi","stroke","resvarcular","uap","diab_retinopathy","diab_nephropathy","dialysis","tia","Hypogly")
  
  out <- lapply(names(outlist), function(x){
    dat <- findCondition(data=LPR,
                         vars="diag",
                         keep=c("pnr","inddto", "uddto"),
                         conditions=outlist[x],
                         match="start")
    setkeyv(dat,c("pnr","X","inddto", "uddto"))
    dat <- dat[,.SD[1],by=.(pnr,X,inddto)]  #no multiple entries of a diag on the same date
    #Get rid of all outcomes before index, not relevant
    dat <- data[,.(pnr,index.date)][dat,on=.(pnr)]
    dat <- dat[inddto>=index.date]
    dat[,index.date:=NULL]
    return(dat)
  })
  
  names(out) <- names(outlist)
  
  # Add revascularization (new June2024) - defined by procedures 
  revasc <- findCondition(data=procedures,
                          vars="opr",
                          keep=c("pnr","inddto","uddto"),
                          conditions=procedureslist,
                          match="start")
  
  #Add revas info to out$resvarcular
  out$resvarcular <- revasc[,.(pnr,X="resvarcular",inddto=inddto,uddto=uddto)]
  
  
  # Death: death, cvd death, and cvd death in 2022
  
  #Death
  death <- death[pnr%in%data$pnr]
  
  # Cause specific deaths
  d2 <- importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/Death/t_dodsaarsag_2.sas7bdat",keep=c("pnr","d_dodsdato",
                                                                                                 "c_dodtilgrundl_acme","c_dod_1a","c_dod_1b",
                                                                                                 "c_dod_1c","c_dod_1d","c_dod_21","c_dod_22",
                                                                                                 "c_dod_23","c_dod_24","c_dod_25","c_dod_26",
                                                                                                 "c_dod_27","c_dod_28"
  ),
  filter=data[,.(pnr)])
  
  d2[,cvd_death:=apply(d2[,.(c_dodtilgrundl_acme,c_dod_1a,c_dod_1b,c_dod_1c,
                             c_dod_1d,c_dod_21,c_dod_22,c_dod_23,c_dod_24,
                             c_dod_25,c_dod_26,c_dod_27,c_dod_28)],
                       1,function(x){any(grepl("^I",x))})]
  
  d2[cvd_death==TRUE,doddato_cvd:=d_dodsdato]
  d2 <- d2[!is.na(doddato_cvd),.(pnr,doddato_cvd)]
  
  # CVD death during 2022 now 23?  ----
  death[,year:=year(doddato)]
  death22 <- death[year==2023,.(pnr,joindate=doddato,doddato )]
  setkey(death22,pnr,joindate)
  
  # Defining CVD
  vars <- c("afib","copd","cvd","heart.failure","hypertension","ihd","mi","peripheral.vascular.disease") #names(dt_comorb)[3:length(dt_comorb)]
  
  # Roll them on: Prevalent disease within two years prior to death
  cvd22 <- Reduce(function(x,y){merge(x,y,all.x=T,by="pnr")},
                  (lapply(1:length(vars), function(x){
                    dt_comorb_wide[,joindate:=get(vars[x])]
                    setkey(dt_comorb_wide,pnr,joindate)
                    dat <- dt_comorb_wide[,.(pnr,get(vars[x]),joindate)][death22,roll=365.24*5]
                    setnames(dat,"V2",vars[x])
                    return(dat[,1:2])
                  })))
  
  for(x in vars) cvd22[,(x):=!is.na(get(x))]
  cvd22[,cvd_death22:=apply(cvd22[,c("afib","copd","cvd","heart.failure","hypertension","ihd","mi","peripheral.vascular.disease")],1,function(x)any(x==TRUE))]
  
  death22 <- cvd22[,.(pnr,cvd_death22)][death22,on=.(pnr)]
  death22[cvd_death22==TRUE,doddato_cvd:=doddato]
  death22 <- death22[!is.na(doddato_cvd),.(pnr,doddato_cvd)]
  
  d2 <- rbind(d2,death22)
  
  #Non CVD death
  
  d3 <- death[!pnr %in% d2$pnr]
  
  #Naming necessary to do widen_ functions
  return(c(out,list(death=death[,.(pnr,X="death",inddto=doddato,uddto=doddato)],
                    cvddeath=d2[,.(pnr,X="cvddeath",inddto=doddato_cvd,uddto=doddato_cvd)],
                    nonCVDdeath=d3[,.(pnr,X="nonCVDdeath",inddto=doddato,uddto=doddato)])))
  
}







# fx.get.outcomes <- function(data,LPR,comorblist,death,dt_comorb_wide) {
#   
#   # tar_load(pnrlist.EMPAREG)
#   # data=pnrlist.EMPAREG
#   # tar_load(comorblist)
#   # tar_load(dt_comorb_wide)
#   # tar_load(death)
#   # tar_load(LPR)
# 
# dt_comorb=dt_comorb_wide
# dt_comorb_wide <- dt_comorb_wide[!is.na(pnr)] #One pnr is na. This is not the case in the final population.
# 
# outlist <- list(comorblist$heart.failure,comorblist$mi,comorblist$stroke,comorblist$resvarcular,comorblist$uap,comorblist$diab_retinopathy,comorblist$diab_nefropathy,comorblist$dialysis,comorblist$tia)
# names(outlist) <- c("heart.failure","mi","stroke","resvarcular","uap","diab_retinopathy","diab_nephropathy","dialysis","tia") 
# 
# out <- lapply(names(outlist), function(x){
#   dat <- findCondition(data=LPR,
#                        vars="diag",
#                        keep=c("pnr","inddto", "uddto"),
#                        conditions=outlist[x],
#                        match="start")
#   setkeyv(dat,c("pnr","X","inddto", "uddto"))
#   dat <- dat[,.SD[1],by=.(pnr,X,inddto)]  #no multiple entries of a diag on the same date
#   #Get rid of all outcomes before index, not relevant
#   dat <- data[,.(pnr,index.date)][dat,on=.(pnr)]
#   dat <- dat[inddto>=index.date]
#   dat[,index.date:=NULL]
#   return(dat)
# })
# 
# names(out) <- names(outlist)
# 
# 
# # Death: death, cvd death, and cvd death in 2022 
# 
# #Death 
# death <- death[pnr%in%data$pnr]
# 
# # Cause specific deaths
# d2 <- importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/Death/t_dodsaarsag_2.sas7bdat",keep=c("pnr","d_dodsdato",
#                                     "c_dodtilgrundl_acme","c_dod_1a","c_dod_1b",
#                                     "c_dod_1c","c_dod_1d","c_dod_21","c_dod_22",
#                                     "c_dod_23","c_dod_24","c_dod_25","c_dod_26",
#                                     "c_dod_27","c_dod_28"
# ),
# filter=data[,.(pnr)])
# 
# d2[,cvd_death:=apply(d2[,.(c_dodtilgrundl_acme,c_dod_1a,c_dod_1b,c_dod_1c,
#                            c_dod_1d,c_dod_21,c_dod_22,c_dod_23,c_dod_24,
#                            c_dod_25,c_dod_26,c_dod_27,c_dod_28)],
#                      1,function(x){any(grepl("^I",x))})]
# 
# d2[cvd_death==TRUE,doddato_cvd:=d_dodsdato]
# d2 <- d2[!is.na(doddato_cvd),.(pnr,doddato_cvd)]
# 
# # CVD death during 2022  ----
# death[,year:=year(doddato)]
# death22 <- death[year==2022,.(pnr,joindate=doddato,doddato )]
# setkey(death22,pnr,joindate)
# 
# # Defining CVD 
# vars <- c("afib","copd","cvd","heart.failure","hypertension","ihd","mi","peripheral.vascular.disease") #names(dt_comorb)[3:length(dt_comorb)]
# 
# # Roll them on: Prevalent disease within two years prior to death 
# cvd22 <- Reduce(function(x,y){merge(x,y,all.x=T,by="pnr")},
#                 (lapply(1:length(vars), function(x){
#                   dt_comorb_wide[,joindate:=get(vars[x])]
#                   setkey(dt_comorb_wide,pnr,joindate)
#                   dat <- dt_comorb_wide[,.(pnr,get(vars[x]),joindate)][death22,roll=365.24*2]
#                   setnames(dat,"V2",vars[x])
#                   return(dat[,1:2])
#                 })))
# 
# for(x in vars) cvd22[,(x):=!is.na(get(x))]
# cvd22[,cvd_death22:=apply(cvd22[,c("afib","copd","cvd","heart.failure","hypertension","ihd","mi","peripheral.vascular.disease")],1,function(x)any(x==TRUE))]
# 
# death22 <- cvd22[,.(pnr,cvd_death22)][death22,on=.(pnr)]
# death22[cvd_death22==TRUE,doddato_cvd:=doddato]
# death22 <- death22[!is.na(doddato_cvd),.(pnr,doddato_cvd)]
# 
# d2 <- rbind(d2,death22)
# 
# #Naming necessary to do widen_ functions 
# return(c(out,list(death=death[,.(pnr,X="death",inddto=doddato,uddto=doddato)],cvddeath=d2[,.(pnr,X="cvddeath",inddto=doddato_cvd,uddto=doddato_cvd)])))
# 
# } 





