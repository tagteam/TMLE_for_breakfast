ltmle_data_setup <- function(df,alldiab, allmedicine){

  ######################################################################
  #
  # Prepare for time dependent variables
  #
  ######################################################################
  base_dem <- studypop
  base_dem[,inn:=start] # start of first secondary treatment
  base_dem[,out:=pmin(re_stroke, re_MI, doddato,emi_dato,as.Date("2021-12-31"),na.rm=TRUE)] # last date
  base_dem[,p_outcome := pmin(re_stroke, re_MI, doddato, na.rm = TRUE)]
  base_dem[,last_date:=out] #keep track of last date for def of censoring
  base_dem <- base_dem[out>=inn] # 1 patients died prior to picking up medication
  base_dem[,dummy:=1] #necessary dummy for splitting
  base_dem <- base_dem[,c('pnr','start','sex','fdato','emi_dato','p_outcome','inn','out','last_date','dummy')]
  
  ######################################################################
  #
  # Antidiabetisk medicin
  #
  ######################################################################
  alldiab <- readRDS('_targets/objects/alldiab.rds') #all the antidiab drug consumption
  alldiab[,value:= 1]
  
  ######################################################################
  #
  # Other baseline medicin
  #
  ######################################################################
  
  allmedicine
  
  ######################################################################
  #
  # Comorbiditet
  #
  ######################################################################
  
  cov_comorb <- readRDS('_targets/objects/cov_comorb')
  
  ######################################################################
  
  ######################################################################
  #
  # Laboratory values
  #
  ######################################################################
  
  lab <- readRDS('_targets/objects/lab')
  
  ######################################################################
  
  lab_hba1c <- lab[class=='HBA1C']
  setkeyv(lab_hba1c, c('pnr', 'start'))
  lab_hba1c[,end:= start + 365.25]  
  lab_hba1c[,end2:=shift(start,type="lead"),by=c("pnr")]
  lab_hba1c[,end:=pmin(end,end2, na.rm=TRUE)]
  lab_hba1c[,end2:=NULL]
  #Sidste værdi skal tælle for et år.
  
  ######################################################################
  #
  # Splitting 
  #
  ######################################################################
  
  # Select part of base_dem that needs splitting
  # Keep only variables defining intervals and dates that are part of splitting - plus dummy! and dementia
  base_dem_split <- base_dem
  # Split by time of covariates
  time_dem <- lexisTwo(base_dem_split,cov_comorb,invars = c("pnr","inn","out","dummy"),
                       splitvars = c('afib', 'cancer', 'copd','heart.failure','hypertension','peripheral.vascular.disease','renal.disease','depression','othermental' ))
  
  #Split by diabetes treatment
  time_dem <- lexisFromTo(time_dem,alldiab,invars = c("pnr","inn","out","dummy"),
                          splitvars=c("pnr","start","end","value","drugdiab"))
  
  #split by hba1c value
  time_dem <- lexisFromTo(time_dem,lab_hba1c,invars = c("pnr","inn","out","dummy"),
                          splitvars=c("pnr","start","end","res","class"))
  
  # Split by calender time
  time_dem <- lexisSeq(time_dem,invars = c("pnr","inn","out","dummy"),
                       varname = NULL,
                       splitvector = as.Date(c("2012-01-01")),
                       value="calender",format="vector")
  
  # Split in 6 month periods since baseline
  time_dem <- lexisSeq(time_dem,invars = c("pnr","inn","out","dummy"),
                       varname="start",splitvector = c(0,25*365,180),
                       format="seq",value="splittime")
  
  # Split in 1 year age intervals
  time_dem <- lexisSeq(time_dem,invars = c("pnr","inn","out","dummy"),
                       varname="fdato",splitvector=c(0,100*365,1*365),
                       format="seq",value="age")
  time_dem[,age:=age*1]
  
  # Outcome node
  time_dem[,event:=0]
  time_dem[!is.na(p_outcome) & p_outcome>=inn & p_outcome<=out,event:=1]
  time_dem[,event:=max(event),by=c("pnr","splittime")]
  
  # Censoring node
  time_dem[,censor:=0]
  time_dem[out==last_date & event!=1, censor:=1]
  time_dem[,censor:=max(censor),by=c("pnr","splittime")]
  
  #Treatment group categorization
  time_dem[, glp1 := 0]
  time_dem[semagltd==1|liraglutd==1|exenatd==1|dulagltd==1|lixisenatd==1,glp1:= 1]
  
  time_dem[, other := 0]
  time_dem[repaglinid==1,other:= 1]
  
  #Hba1c categorisation - see SAP for motivation and justification
  time_dem[,HBA1C:= as.numeric(HBA1C)]
  time_dem[,HBA1C_cat:= cut(HBA1C,
                            breaks= c(0,1,20,42,48,53,58,70,202), 
                            right = F, 
                            include.lowest = T)]
  time_dem[,.N, by = HBA1C_cat]
  time_dem[,HBA1C_cat := factor(HBA1C_cat, levels = c("[0,1)","[20,42)","[42,48)","[48,53)","[53,58)","[58,70)","[70,202]"), labels = c("missing", "[20,42)","[42,48)","[48,53)","[53,58)","[58,70)",">70"))]
  
  ###############################################################
  #
  # Going from long to wide format
  #
  ###############################################################
  #Add empty records so that all individuals complete records
  setkeyv(time_dem,c("pnr","splittime"))
  pnr2 <- unique(time_dem[,pnr])
  splittime <- unique(time_dem[,splittime])
  pnr3 <- data.table(pnr=rep(pnr2,each=length(splittime)),splittime=rep(0:(length(splittime)-1),length(pnr2)))
  time_dem <- merge(time_dem,pnr3,by=c("pnr","splittime"),all=TRUE)
  
  # Fill event with 1 and censor with 1
  time_dem[,event:=nafill(event,type="locf")]
  time_dem[,censor:=nafill(censor,type="locf")]
  
  #From long to wide format
  time_dem_wide <- time_dem[,.SD[1],by=c("pnr","splittime")]
  time_dem_wide2 <- dcast(time_dem_wide,pnr~splittime, value.var = c("age", "calender","HBA1C", "HBA1C_cat","sglt2_inhib","glp1", "semagltd","sulfonylurea","insulin","dpp4_inhib","otherglp", "other","thiazolodinidion","afib","copd", "heart.failure", "hypertension", "peripheral.vascular.disease", "renal.disease", "depression", "othermental","event", "censor"))
  
  colnames <- colnames(time_dem_wide2)
  
  #Only first 10 intervals - customization of the number of time intervals you wish to apply
  col2remove <- c(grep("_11", colnames(time_dem_wide2), fixed = T), grep("_12", colnames(time_dem_wide2), fixed = T), grep("_13", colnames(time_dem_wide2), fixed = T), grep("_14", colnames(time_dem_wide2), fixed = T), grep("_15", colnames(time_dem_wide2), fixed = T), grep("_16", colnames(time_dem_wide2), fixed = T), grep("_17", colnames(time_dem_wide2), fixed = T), grep("_18", colnames(time_dem_wide2), fixed = T), grep("_19", colnames(time_dem_wide2), fixed = T), grep("_20", colnames(time_dem_wide2), fixed = T), grep("_21", colnames(time_dem_wide2), fixed = T), grep("_22", colnames(time_dem_wide2), fixed = T), grep("_23", colnames(time_dem_wide2), fixed = T), grep("_24", colnames(time_dem_wide2), fixed = T), grep("_25", colnames(time_dem_wide2), fixed = T))
  
  col2remove <- sort(col2remove)
  col2remove <- colnames[col2remove]
  time_dem_wide2 <- time_dem_wide2[,.SD, .SDcols = !col2remove]
  
  #Merge baseline values
  
  time_dem_wide2 <- merge(time_dem_wide2,
                          studypop[, c(
                            "pnr",
                            "sex",
                            "code5txt",
                            "bb_180",
                            "ccb_180",
                            "rasi_180",
                            "thiazid_180",
                            "loop_180",
                            "mra_180",
                            "digoxin_180",
                            "statin_180",
                            "asa_180",
                            "adpi_180",
                            "vka_180",
                            "noac_180",
                            "inh_c_180",
                            "inh_b2_180",
                            "inh_musc_180",
                            "antidep_180",
                            "antipsyc_180", 
                            "T_chol", 
                            "HDL_chol", 
                            "LDL_chol",
                            "CREA",
                            "egfr"
                          )],
                          by = "pnr", all.x = TRUE)
  
  return(time_dem_wide2)
}