get_procedures <- function(pnrlist){
  #tar_load(pnrlist)
  
  # OLD 
  #fyi opr_old is prior to 1995 and not relevant 
  opr2 <- importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/LPR/opr.sas7bdat",
                   keep=c("pnr","inddto","uddto","opr"),
                   filter=pnrlist)
  # NEW 
  kontakter <- importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/LPR/kontakter.sas7bdat",
                         keep=c("dw_ek_kontakt","cpr","dato_start","dato_slut"),filter=pnrlist[,.(cpr=pnr)])
  filterlist <- unique(kontakter[,.(dw_ek_kontakt)])
  pro_k <- importSAS("X:/Data/Rawdata_Hurtig/703740/Grunddata/LPR/procedurer_kirurgi.sas7bdat",
                     filter=filterlist[,.(dw_ek_kontakt)],
                     keep=c("dw_ek_kontakt","procedurekode"))
  
  opr3 <- kontakter[,.(dw_ek_kontakt,inddto=dato_start,uddto=dato_slut,pnr=cpr)][pro_k,on=.(dw_ek_kontakt)] #to get the date 
  opr <- unique(rbind(opr2,opr3[,.(pnr,inddto,uddto,opr=procedurekode)]))
  
  return(opr)
  
}


# #Defining revascularization by procedures 
# revasc <- findCondition(data=opr,
#                         vars="opr",
#                         keep=c("pnr","inddto","opr"),
#                         conditions=procedureslist,
#                         match="start")
