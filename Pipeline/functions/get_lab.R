#Changes. all labs from 1-9 are gone. Only lab 10. Only outcommented. obs an Rbind code downstream is also outcommented and instead lab <- lab1 is inserted.
#Now changed back again

get_lab <- function(pnrlist,
                    ddv_dia,nobs,hba.lower,hba.upper) {  #,hba.lower,hba.upper ##lab1,lab2,lab3,lab4,lab5,lab6,lab7,lab8,lab9,lab10
  
  #tar_load(c(lab1,lab2,lab3,lab4,lab5,lab6,lab7,lab8))
  #tar_load(pnrlist)
  #nobs=10000
  
  lab1="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/Kbh amt/kbhamt_results.sas7bdat" #blodprove_kbhamt
  lab2="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/KPLL/kpll2013.sas7bdat" #KPLL
  lab3="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/Reg_Nord/regnord_labka_2006_2007.sas7bdat"#blodprove_nord0607
  lab4="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/Reg_Nord/regnord_labka_2008_2009.sas7bdat"
  lab5="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/Reg_Nord/regnord_labka_2010_2011.sas7bdat"
  lab6="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/Reg_Nord/regnord_labka_2012_2013.sas7bdat"
  lab7="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/Reg_Nord/regnord_analyser_labkaii.sas7bdat"
  lab8="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/Reg_Nord/regnord_labkai_final_2.sas7bdat"
  lab9="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/Roskilde/roskilde_blodprove_roskilde.sas7bdat"
  lab10="X:/Data/Rawdata_Hurtig/703740/Grunddata/laboratory/lab_forsker.sas7bdat" #NEW
  #NEW 
  
  #lab1: KBH amt: chol is difficult
  hbc1 <- importSAS(lab1,
                    where="testcode contains 'HBA' or testcode contains 'CREA' or batterycode contains'CHOL'or batterycode contains 'LDL'or batterycode contains 'HDL'",
                    character.vars=c("pnr","testresult","result_normal_range"),filter=pnrlist,obs=nobs)

  suppressWarnings(hbc1[,testresult:=as.numeric(gsub(">","",testresult))])
  hbc1 <- hbc1[!is.na(testresult)]
  hbc1[,start:=as.Date(collectdate)]
  hbc1[grepl('^CREA',testcode),class:='CREA']
  hbc1[grepl('^CHOL',testcode),class:='T_chol']
  hbc1[grepl('^LDL',testcode)&!grepl('LDLED',testcode),class:='LDL_chol']
  hbc1[grepl('^HDL',testcode),class:='HDL_chol']
  hbc1[grepl('^HBA1C',testcode),class:='HBA1C']
  hbc1[class=='HBA1C'& testresult<1,testresult:=testresult*100]
  hbc1[class=='HBA1C',testresult:=testresult*10.93-23.5]
  hbc1 <- hbc1[!is.na(class)]
  hbc1 <- hbc1[,.(pnr,class,res=testresult,start)]

  #Lab2: KPLL - strange numbers for hba1c
  hbc2 <- importSAS(lab2,
                    where="col3='CR' or col3 in ('CHO','LDL','HDL')",filter=pnrlist,obs=nobs)

  suppressWarnings(hbc2[,res:=as.numeric(gsub(",",".",col5))])
  hbc2[,start:=as.Date(col2,format="%d-%m-%y")]
  hbc2[col3=="CR",class:="CREA"]
  hbc2[col3=="CHO",class:="T_CHOL"]
  hbc2[col3=="LDL",class:="LDL_chol"]
  hbc2[col3=="DL",class:="HDL_chol"]

  hbc2 <- hbc2[,.(pnr,class,res,start)]


  #Lab3-7: region nord
  hbc3 <- importSAS(lab3,filter=pnrlist,obs=nobs)
  hbc4 <- importSAS(lab4,filter=pnrlist,obs=nobs)
  hbc5 <- importSAS(lab5,filter=pnrlist,obs=nobs)
  hbc6 <- importSAS(lab6,filter=pnrlist,obs=nobs)

  hbc3456 <- rbind(hbc3,hbc4,hbc5,hbc6)

  nord <- importSAS(lab7,character.vars="analyse_id")
  hbc <- merge(hbc3456,nord,by="analyse_id")

  hbc[,resultat:=gsub(">","",resultat)]
  suppressWarnings(hbc[,resultat:=as.numeric(gsub(",",".",resultat))])
  hbc[,start:=as.Date(prv_datoklok)]
  hbc[analysenavn=="P-Creatinin",class:="CREA"]
  hbc[grepl("DCCT",component)|grepl("IFCC",component),class:="HBA1C"]
  hbc[grepl("DCCT",component)&resultat<1,resultat:=resultat*100]
  hbc[grepl("DCCT",component),resultat:=resultat*10.93-23.5]
  hbc[npucode=="NPU01566",class:="T_chol"]
  hbc[npucode=="NPU01568",class:="LDL_chol"]
  hbc[npucode=="NPU01567",class:="HDL_chol"]

  hbc <- hbc[,.(pnr,class,res=resultat,start)]
  hbc <- hbc[!is.na(class)]


  hbc8 <- importSAS(lab8,where="component='Hemoglobin A1c' or component='Creatinine' or npucode=:'NPU0156'",
                    filter=pnrlist,obs=nobs)

  suppressWarnings(hbc8[,res:=as.numeric(result)])
  hbc8[,start:=as.Date(sampledate,format="%d.%m.%Y")]
  hbc8[component=="Creatinin",class:="CREA"]
  hbc8[component=="Hemoglobin A1c",class:="HBA1C"]

  hbc8[class=='HBA1C'& res<1,res:=res*100]
  hbc8[class=='HBA1C',res:=res*10.93-23.5]
  hbc8[npucode=="NPU01566",class:="T_chol"]
  hbc8[npucode=="NPU01568",class:="LDL_chol"]
  hbc8[npucode=="NPU01567",class:="HDL_chol"]

  hbc8 <- hbc8[,.(pnr,class,res,start)]

  #Lab 9: Roskilde

  hbc9 <- importSAS(lab9,where="analyse in('HBA1C','CREA','CHOL','LDL','HDL')",filter=pnrlist,obs=nobs)
  suppressWarnings(hbc9[,resultat:=as.numeric(gsub("=","",resultat))])
  hbc9[,start:=as.Date(dato,format="%d%B%y")]

  hbc9[analyse=="CREA",class:="CREA"]
  hbc9[analyse=="HBA1C",class:="HBA1C"]
  hbc9[analyse=="HBA1C"&resultat<1,resultat:=resultat*100]
  hbc9[analyse=="HBA1C",resultat:=resultat*10.93-23.5]

  hbc9[analyse=="CHOL",class:="T_chol"]
  hbc9[analyse=="LDL",class:="LDL_chol"]
  hbc9[analyse=="HDL",class:="HDL_chol"]

  hbc9 <- hbc9[,.(pnr,class,res=resultat,start)]

  
  #Labka
  lab <- importSAS(lab10,
                   where="analysiscode  in ('NPU27300','NPU03835','NPU02307','NPU18016','NPU18105','NPU01807',
      'NPU01566','NPU01567','NPU01568','NPU03620','NPU04094','NPU19680','NPU19661','NPU03918')",
                   filter=pnrlist[,.(pnr=pnr)])
  
  
  setnames(lab,"patient_cpr","pnr",skip_absent = TRUE)
  lab[,res:=as.numeric(value)]
  setnames(lab,"samplingdate","start")
  
  lab[analysiscode %in% c('NPU18016','NPU18105','NPU01807'),class:='CREA']
  lab[analysiscode %in% c('NPU27300','NPU03835','NPU02307'),class:='HBA1C']
  lab[analysiscode=='NPU03835' & res<1,res:=res*100]
  lab[analysiscode=='NPU03835', res:=res*10.93-23.5]
  lab[analysiscode=='NPU01566',class:='T_chol']
  lab[analysiscode=='NPU01568',class:='LDL_chol']
  lab[analysiscode=='NPU01567',class:='HDL_chol']
  lab[analysiscode %in% c('NPU03620','NPU04094'),class:='TRIGLYD']
  lab[analysiscode=='NPU19680',class:='U-alb']
  lab[analysiscode=='NPU19661'|analysiscode=='NPU03918',class:='UACR']
  
  lab[class=='HBA1C' & res<hba.lower, res:=NA]
  lab[class=="HBA1C"&res>hba.upper,res:=NA]
  lab[class=="LDL_chol"&res>10,res:=NA]
  lab[class=="TRIGLYD"&res>13,res:=NA]
  lab[class=="CREA"&res>1800,res:=NA]
  lab[class=="CREA"&res<10,res:=NA]
  
  lab <- unique(lab[!is.na(res),.(pnr,class,res,start)])
  
  
  lab1 <- rbind(hbc1,hbc2,hbc,hbc8,hbc9,lab)
  
  # lab1 <- lab
  
  # Laboratory measurements from DDV
  
  
  hba <- ddv_dia[,.(pnr,res=hba1c,start=hba1c_dato,class="HBA1C")]
  ldl <- ddv_dia[,.(pnr,res=ldlcholesterol,start=lipids_dato,class="LDL_chol")]
  chol <- ddv_dia[,.(pnr,res=totalcholesterol,start=lipids_dato,class="CHOL")]
  crea <- ddv_dia[,.(pnr,res=plasmakreatinin,start=plasmakreatinin_dato,class="CREA")]
  
  labdia <- rbind(hba,ldl,chol,crea)
  labdia[!is.na(res)]
  
  labcomb <- unique(rbind(lab1,labdia))
  setkey(labcomb,pnr,start)
  
  
  return(labcomb)
  
  
}



