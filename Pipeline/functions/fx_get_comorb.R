fx.get.comorb <- function(data,LPR,comorblist,lmdb,procedures,procedureslist){
  # tar_load(dt_index)
  # data=dt_index
  # tar_load(LPR)
  # tar_load(comorblist)
  # tar_load(dlmdb)
  
  out <- lapply(names(comorblist), function(x){
    cov <- findCondition(data=LPR,
                         vars="diag",
                         keep=c("pnr","inddto", "uddto"),
                         conditions=comorblist[x],
                         match="start")
    
    setkeyv(cov,c("pnr","X","inddto", "uddto"))
    cov <- cov[,.SD[1],by=.(pnr,X,inddto)]  #no multiple entries of a diag on the same date 
    return(cov)
  })
  
  names(out) <- names(comorblist)
  
  #Defining revascularization by procedures 
  revasc <- findCondition(data=procedures,
                          vars="opr",
                          keep=c("pnr","inddto","uddto"),
                          conditions=procedureslist,
                          match="start")
  #Add revas info to out$resvarcular
  out$resvarcular <- revasc[,.(pnr,X="resvarcular",inddto=inddto,uddto=uddto)]
  
  
  #Defining hypertension by medication
  hyp <- hypertensionMedication(lmdb,vars=c("pnr","atc","eksd"))
  hyp <- hyp[,.(pnr,hypertension)]
  #Add hyp info to out$hypertension
  out$hypertension <- rbind(out$hypertension,hyp[,.(pnr,X="hypertension",inddto=hypertension,uddto=as.Date(NA))])
  
  return(out)
  
}




# fx.get.comorb <- function(data,LPR,comorblist,lmdb){
#   # tar_load(dt_index)
#   # data=dt_index
#   # tar_load(LPR)
#   # tar_load(comorblist)
#   # tar_load(lmdb)
#   # tar_load(dt_comorb)
#   
#   out <- lapply(names(comorblist), function(x){
#     cov <- findCondition(data=LPR,
#                        vars="diag",
#                        keep=c("pnr","inddto", "uddto"),
#                        conditions=comorblist[x],
#                        match="start")
#     setkeyv(cov,c("pnr","X","inddto", "uddto"))
#     cov <- cov[,.SD[1],by=.(pnr,X,inddto)]  #no multiple entries of a diag on the same date
#     return(cov)
#   })
#   
#   names(out) <- names(comorblist)
#   
#   
#   #Defining hypertension by medication
#   hyp <- hypertensionMedication(lmdb,vars=c("pnr","atc","eksd"))
#   hyp <- hyp[,.(pnr,hypertension)]
#   #Add hyp info to out$hypertension
#   out$hypertension <- rbind(out$hypertension,hyp[,.(pnr,X="hypertension",inddto=hypertension,uddto=as.Date(NA))])
#   
#   return(out)
#   
# }


