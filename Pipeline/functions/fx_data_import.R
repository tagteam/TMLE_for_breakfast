#Targets data functions.
#supplements targets file to call all functions. This file is used for data imports


# tar_load(dt_deg_glg)
# tar_load(pnrlist)
# tar_load(diamed_list)


# Dataset of only pnr and index date and specify study period of interest
fx.index.population <- function(dt_deg_glg) {
  dt <- unique(dt_deg_glg[,.(pnr,index.date=index_date,arm)]) #start_secondline defined by degludec or glargine
  dt[,year:=year(index.date)]
  dt <- dt[year>2011] #HERE WE SPECIFY STUDY PERIOD OF INTEREST
  return(dt)
}

#Get all medications
fx.get.lmdb <- function(pnrlist) { # Get medications from 2009-2022
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/medication/',
                         '^lmdb20(09|1[0-9]|2[0-2]).*',full.names=TRUE) # lmdb files 2009-2022: obs: co-medication
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- read_sas(filelist[x],col_select = c("PNR","ATC","eksd","Volume","strnum","PACKSIZE")) 
      #dat <- importSAS(filelist[x],keep=c(("PNR","ATC","eksd","Volume","strnum","PACKSIZE"), obs=1000)
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  dat <- dat[pnr%in%pnrlist$pnr]
  return(dat)
}



fx.get.lmdb.dia <- function(diamedlist) { # Get medications from 2009-2022 Used for defining cohort
  
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/medication/',
                         '^lmdb20(09|1[0-9]|2[0-2]).*',full.names=TRUE) # lmdb files 2009-2022: obs: co-medication
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table","doParallel")) %dopar% {
      dat <- read_sas(filelist[x],col_select = c("PNR","ATC","eksd","Volume","strnum","PACKSIZE")) 
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  
  dat2 <- findCondition(dat,"atc",c("pnr","atc","eksd","volume","packsize"),diamedlist,match="start")
  return(dat2)
}



fx.get.death <- function(pnrlist) {
  importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/death/dod.sas7bdat",
            filter=pnrlist)
}



fx.get.ddv <- function(pnrlist) {
  dat <- importSAS("X:/Data/Rawdata_Hurtig/703740/Eksterne data/dvdd_2023_01_31.sas7bdat",filter=pnrlist)
  return(dat)
}



fx.get.LPR <- function(pnrlist) {
  
  #Lpr1/2
  diag1 <- importSAS('X:/Data/Rawdata_Hurtig/703740/Grunddata/LPR/diag_indl.sas7bdat',
                     where="(substr(diag,1,1) in ('0','1','2','4','5')
                         or substr(diag,1,2) in ('DI','DG','DH','DJ','DM','DB','DK','DE','DN','DC', 'DZ', 'DF','DO','DD'))
                         and pattype= 0",
                     keep=c("pnr","diag","diagtype","inddto","uddto","pattype"),
                     filter = pnrlist)
  
  #LPR3
  kontakt <- importSAS('X:/Data/Rawdata_Hurtig/703740/Grunddata/LPR/kontakter.sas7bdat',
                       keep=c("cpr","DW_EK_KONTAKT","dato_start","tidspunkt_start","dato_slut","tidspunkt_slut"),
                       filter=pnrlist[,.(cpr=pnr)])
  diag2 <- importSAS('X:/Data/Rawdata_Hurtig/703740/Grunddata/LPR/diagnoser.sas7bdat',
                     where="diagnosekode=:'DI2'",keep=c("DW_EK_KONTAKT","diagnosekode"),
                     filter=kontakt[,.(DW_EK_KONTAKT=dw_ek_kontakt)])
  kontakt[,start:=as.POSIXct(paste(dato_start,tidspunkt_start))]
  kontakt[!is.na(dato_slut) & !is.na(tidspunkt_slut),slut:=as.POSIXct(paste(dato_slut,tidspunkt_slut))]
  kontakt[,dif:=slut-start]
  kontakt <- kontakt[dif>60*60*12] # 12 hours as replacement for pattype=0 - not universally relevant, in particular children
  kontakt <- kontakt[,.(cpr,dato_start,dw_ek_kontakt)]
  # diag21 <- merge(diag2,kontakt,all.x = TRUE,by="dw_ek_kontakt")
  diag2 <- diag2[kontakt,on="dw_ek_kontakt"]
  diag2 <- diag2[!is.na(cpr)] #not relevant for real projects
  setnames(diag2,c("cpr","dato_start","diagnosekode"),c("pnr","inddto","diag"))
  diag2[,dw_ek_kontakt:=NULL]
  diag2 <- diag2[pnr%in%pnrlist$pnr]
  diag <- rbind(diag1,diag2,fill=TRUE)
  return(diag)
}




fx.get.bef <- function(pnrlist, death) { # Define a population that enters at year 2000 and exits when
  # they exit bef, immigrate or die or end of 2022
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/Population/',
                         '^bef20(0[0-9]|1[0-9]|2[0-2])12.*',full.names=TRUE) # bef files last 2008-22
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- read_sas(filelist[x],col_select = c("PNR","FOED_DAG","KOEN","REFERENCETID"))
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  setkeyv(dat,c("pnr","referencetid"))
  dat[pnr==shift(pnr) & (referencetid-shift(referencetid)>100),pause:=1]
  dat[,pause:=nafill(pause, type="locf"),by="pnr"] # Mark all after break
  dat <- dat[is.na(pause)] #keep until first pause
  setkeyv(dat,c("pnr","referencetid"))
  dat <- dat[,.(foed_dag=foed_dag,koen=koen,inn=referencetid[1],out=referencetid[.N]+91),by="pnr"]
  dat <- dat[,.SD[1],by="pnr"]
  # Fixing the out to perfectly match next quarter is complicated and for most purposes unnecessary, thus 91 days
  # Get emmigrations out of country:
  vnds <- importSAS('X:/Data/Rawdata_Hurtig/703740/Grunddata/Population/vnds2022.sas7bdat',
                    where="indud_kode='U'")
  temp <- merge(dat,vnds[,.(pnr,haend_dato)],all.x=TRUE,by="pnr")# extra copy since there may be multiple immigrations
  temp <- temp[!is.na(haend_dato) & haend_dato>out-91 & haend_dato<=out] # those that immigrated during last period
  death <- importSAS('X:/Data/Rawdata_Hurtig/703740/Grunddata/Death/dod.sas7bdat')
  dat <- Reduce(function(x,y){merge(x,y,all.x=TRUE,by="pnr")},list(dat,temp[,.(pnr,haend_dato)],death[,.(pnr,doddato)]))
  dat[,out:=pmin(out,doddato,haend_dato)]
  gc()
  dat <- dat[,.(pnr,foed_dag,koen,inn,out)]
  dat <- dat[pnr%in%pnrlist$pnr]
  return(dat)
}

fx.get.kom <- function(pnrlist) {
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/Population/',
                         '^bef20(0912|1[0-9]|2[0-2]).*',full.names=TRUE) # bef files last 2014 and then 2015-22
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- read_sas(filelist[x],col_select = c("PNR","REFERENCETID","KOM"))
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  dat <- dat[pnr%in%pnrlist$pnr] #here we filter
  return(dat)
}



fx.get.education <- function(pnrlist) {
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/social/',
                         '^udda20(14|1[5-9]|2[0-2]).*',full.names=TRUE) # education files 2014-2022
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- setDT(read_sas(filelist[x],col_select = c("PNR","HFAUDD")))
      dat[,year:=x+2013]
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  dat <- merge(dat,edu_code,by="hfaudd",all.x=TRUE)
  dat <- dat[pnr%in%pnrlist$pnr]
  return(dat)
  
}


fx.get.income <- function(pnrlist) {
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/social/',
                         '^ind20(08|09|1[0-9]|2[0-2]).*',full.names=TRUE) # income files 2008-2022
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- setDT(read_sas(filelist[x],col_select = c("PNR","AEKVIVADISP_13")))
      dat[,year:=x+2013]
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  dat <- dat[pnr%in%pnrlist$pnr]
  return(dat)
  
}

