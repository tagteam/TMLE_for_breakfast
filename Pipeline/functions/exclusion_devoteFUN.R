#Exclusion criteria DEVOTE Emulation by PDWY
#

exclusions.devoteFUN <- function(dt_index, immiemi, LPR, lmdb,
                              dt_baseline, dt_comorb_wide, dt_deg_glg,
                              dt_lab, sexage, maxyears, hbalim) {

    if (FALSE){
        tar_load(dt_index)
        tar_load(immiemi)
        tar_load(LPR)
        tar_load(lmdb)
        tar_load(dt_baseline)
        tar_load(dt_deg_glg)
        tar_load(dt_comorb_wide)
        tar_load(dt_lab)
        tar_load(sexage)
        maxyears=20 #number of years looking back from index
        hbalim=53
    }
    # first row of the flowchart data
    a=list("start pop",dt_index[,uniqueN(pnr)])
    ## flowchart=data.frame(Population="start pop",N=dt_index[,uniqueN(pnr)])
    
    #Excluding pt. under the age of 50 at the time they initiated treatment of interest ----
    dt_baseline[ , age := as.numeric((min.start - fdato) / 365.25)] #min start is the same as index date
    over50 <- dt_baseline[age >= 50]
    data1 <- dt_index[pnr %in% over50$pnr]
    b=list("under 50",dt_baseline[,uniqueN(pnr)]-data1[,uniqueN(pnr)])
    rm(over50)




#Having Hba1C of over 53 at baseline (up to 1 year prior to baseline is accepted)  ----
  data2 <- copy(data1)
  dtlab <- dt_lab[class=="HBA1C"&pnr%in%data1$pnr]
  dtlab[,joindate:=start]
  setkey(dtlab,pnr,joindate)
  data2[,joindate:=index.date]
  setkey(data2,pnr,joindate)
  lab <- dtlab[data2,roll=365]
  lab <- lab[!is.na(res)]
  lab <- lab[res>=hbalim]
  data3 <- data2[pnr%in%lab$pnr]
  c=list("No hba or hba<53",data2[,uniqueN(pnr)]-data3[,uniqueN(pnr)])
  rm(data2, lab)
  


# including patients with established CVD or low eGFR or being over 60 and having HT or albuminurea  ----
  
  #Include patients with CVD being over 50 years of age (implicit)
  dt_comorb=copy(dt_comorb_wide)
  index <- data3[ , .(pnr, index.date, joindate = index.date)]
  setkey(index,pnr,joindate)
  comorblist <- list("mi", "ihd", "uap", "stroke", "heart.failure","resvarcular","hypertension")
  
  dtink <-  lapply(comorblist, function(x) {
    dat <- dt_comorb[!is.na(get(x)),.(pnr,joindate=get(x))]
    dat[,(x):=TRUE]
    setkey(dat,pnr,joindate)
    index <<- dat[index,roll=365*maxyears]
    index
  })
  
  dtink <- dtink[[length(dtink)]]
  dtink <- dt_baseline[,.(pnr,fdato)][dtink,on=.(pnr)] 
  dtink[,age:=as.numeric((index.date-fdato)/365.25)] 
  
  #including pt. with egfr between 30 and 60  
  dtlab <- dt_lab[class=="CREA"&pnr%in%data3$pnr]
  dtlab[,joindate:=start]
  setkey(dtlab,pnr,joindate)
  data3[,joindate:=index.date]
  setkey(data3,pnr,joindate)
  lab <- dtlab[data3,roll=365*1]
  lab <- lab[!is.na(res)]
  
    #calculating egfr
    lab <- merge(lab,sexage,by="pnr", all.x=T)
    lab[,age:=as.numeric(floor((start-foed_dag)/365.25))]
    lab[koen == 0, k := 0.7]
    lab[koen == 1, k := 0.9]
    lab[koen == 0, a := -0.329]
    lab[koen == 1, a := -0.411]
    lab[koen == 0, multiplyer := 1.018]
    lab[koen == 1, multiplyer := 1]
    lab[,resmgdl := res/88.4 ]
    lab[, scr := resmgdl/k]
    lab[,min := ifelse(scr<=1,scr,1)]
    lab[,max := ifelse(scr>=1,scr,1)]
    lab[,egfr:=141*(min^a)*(max^-1.209)*(0.993^age)*multiplyer]
  
  lab[,renalfunc:=egfr<60 & egfr>30]
  labtrue <- lab[renalfunc==T]
  dtink <- merge(dtink,labtrue[,.(pnr,renalfunc)],by="pnr",all=T)

  
  
  #Now adding albuminurea 
  dtlab <- dt_lab[class=="UACR"&pnr%in%data3$pnr]
  dtlab[,joindate:=start]
  setkey(dtlab,pnr,joindate)
  data3[,joindate:=index.date]
  setkey(data3,pnr,joindate)
  labalb <- dtlab[data3,roll=365*1]
  labalb <- labalb[!is.na(res)]
  labalb[,alb:=res>=30]
  labalb <- labalb[alb==T]
  dtink <- merge(dtink,labalb[,.(pnr,alb)],by="pnr",all=T)
  
  #Calculating Age
  dtink[,age:=as.numeric(floor((index.date-fdato)/365.25))]
  
  
  #adding CVD and egfr true together AND including being over 60 having HT or albuminurea 
  dtink[ , include1:=(heart.failure==T|ihd==T|mi==T|
                        uap==T|stroke==T|resvarcular==T|renalfunc==T)]
  dtink[,include2:=include1==T | (age>60 & hypertension==T)]
  dtink[,include3:=include2==T | (age>60 & alb==T)]
  dtink <- dtink[include3==T]
  
  
  
  data4 <- data3[pnr%in%dtink$pnr]
  d=list("No CVD or age 60 and hypertention/renal",data3[,uniqueN(pnr)]-data4[,uniqueN(pnr)])
  rm(data3, dtink)
  
  
#Exlusions ----  
  
  
  

  
  # End-stage liver disease within 10 years prior to index, K72 ----
  end_liver <- LPR[grepl("^DK72",diag), .(pnr, diag, inddto, joindate = inddto)]
  setkey(end_liver, pnr, joindate)
  data4[, joindate := index.date]
  setkey(data4, pnr, joindate)
  dtliv <- end_liver[data4, roll = 365.24 * maxyears]
  dtliv <- dtliv[!is.na(diag)]
  data5 <- data4[!pnr %in% dtliv$pnr]
  e=list("End-stage liver disease, K72",data4[,uniqueN(pnr)]-data5[,uniqueN(pnr)])
  rm(data4, dtliv, end_liver)
  
  
 
  
  
  # Any cancer C00-C97 10 years prior to index including MDS (DD46) ----
  cancer <- LPR[grepl("^DC",diag) | grepl("^DD46",diag) ,.(pnr,diag,inddto,joindate=inddto)]
  cancer <- cancer[!(grepl("^DC98",diag) | grepl("^DC99",diag))] # Exclude some (could be coded more nicely)
  setkey(cancer,pnr,joindate)
  data5[,joindate:=index.date]
  setkey(data5,pnr,joindate)
  cancer <- cancer[data5,roll=365.24*5]
  cancer <- cancer[!is.na(diag)]
  data6 <- data5[!pnr%in%cancer$pnr]
  e=list("Cancer C00-C97",data5[,uniqueN(pnr)]-data6[,uniqueN(pnr)])
  rm(data5, cancer)
  
  

    
  #Exclude occurrence of AMI or stroke or TCI
  mi60days <- LPR[grepl(paste("DI200", "DI21", "DI22", "DI60", "DI61", "DI63", "DI64", "DG45", sep = "|"), diag),
                   .(pnr, diag, inddto, joindate=inddto)]
  mi60days[, joindate := inddto]
  data6[, joindate := index.date]
  setkey(mi60days, pnr, joindate)
  setkey(data6, pnr, joindate)
  mi60days <- mi60days[data6, roll = 60]
  mi60days <- mi60days[!is.na(diag)]
  data7 <- data6[!pnr %in% mi60days$pnr]
  g = list("MI or Stroke 120 days prior to index", data6[, uniqueN(pnr) - data7[, uniqueN(pnr)]])
  rm(data6, mi60days)
  
  # Immigration within 10 years prior to baseline ----
  bi <- unique(immiemi[indud_kode=="I",.(pnr,joindate=haend_dato,immi=T)])
  setkey(bi,pnr,joindate)
  data7[,joindate:=index.date]
  setkey(data7,pnr,joindate)
  immi <- bi[data7,roll=365.25*maxyears]
  immi <- immi[!is.na(immi)]
  data8 <- data7[!pnr%in%immi$pnr]
  h=list("Immigrated 0-10 years",data7[,uniqueN(pnr)]-data8[,uniqueN(pnr)])
  
  
  # e-GFR of less than 30 is excluded
  data9 <- copy(data8)
  lablow <- lab[egfr <30]
  data10 <- data9[!pnr%in%lablow$pnr]
  data10 <- merge(data10,lab[,.(pnr,egfr)],all.x=TRUE,all.y=FALSE,by="pnr")
  i=list("eGFR < 30",data9[,uniqueN(pnr)]-data10[,uniqueN(pnr)])
  rm(data9, lab, lablow)
  
  #exclude DM1
  dm1 <- LPR[grepl("^DE10",diag), .(pnr, diag, inddto, joindate = inddto)]
  setkey(dm1, pnr, joindate)
  data10[, joindate := index.date]
  setkey(data10, pnr, joindate)
  dtliv <- dm1[data10, roll = 365.24 * maxyears]
  dtliv <- dtliv[!is.na(diag)]
  data11 <- data10[!pnr %in% dtliv$pnr]
  j=list("DM1",data10[,uniqueN(pnr)]-data11[,uniqueN(pnr)])
  rm(data10, dtliv, dm1)
  

   
  
    #Making an overview of exclusions
    tab.exclusion <- data.table(rbind(b,c,d,e,g,h,i,j))
    n = list("Final study population", (as.numeric(a[2]) - tab.exclusion[, sum(as.numeric(V2))]))
    tab.exclusions <- data.table(rbind(a,b,c,d,e,g,h,i,j,n))
    ## fwrite(tab.exclusions,("data/tab_ex.csv"))
    out <- data11[,.(arm,pnr,index.date,egfr)]
    attr(out,"flowchart") <- tab.exclusions
    rm(a,b,c,d,e,g,h,i,j,n)
    return(out)  
} 
  
  





