#tar_load(pnrlist)

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


