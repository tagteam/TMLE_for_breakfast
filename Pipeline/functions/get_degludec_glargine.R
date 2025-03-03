get_deg_glg <- function(lmdb_dia,length_prescrip){  #Categorize drugclasses 
  
  #length_prescrip=180
  
  #One prescription per day per drug
  setnames(lmdb_dia,c("X","eksd"),c("drugdiab","start"))
  setkeyv(lmdb_dia,c("pnr","start","drugdiab")) 
  lmdb_dia <- lmdb_dia[,.SD[1],.(pnr,start,drugdiab)] 
  
  #fix end dates 
  #Treatment period - assumed length_prescrip months OR new prescription 
  lmdb_dia[,end:=start+length_prescrip] #enddate 
  lmdb_dia[,end2:=shift(start,type="lead"),by=c("pnr","drugdiab")] #
  lmdb_dia[,end:=pmin(end,end2, na.rm=TRUE)]
  lmdb_dia[,end2:=NULL]
  
  # Fix categorization of drugs and make new variable
  lmdb_dia[,drugclass:=drugdiab]
  lmdb_dia[drugdiab%in%c("liraglutd","exenatd","dulagltd","semagltd","otherglp"),drugclass:="glp1"]
  lmdb_dia[drugdiab%in%c("thiazolodinidion","repaglinid","lixisenatd","alfa_glusidase_inhib"),drugclass:="other"]
  
  # Fix min and max dates 
  ## Each PNR gets a TRUE in minglg if EVER collected a Glargine product
  lmdb_dia[drugclass=="glargine",minglg:=start,pnr]
  suppressWarnings(lmdb_dia[,minglg:=min(minglg,na.rm=T),pnr]) #supress warnings is unnecessary, but it just ugly
  
  ## Each PNR gets a TRUE in mindeg if EVER collected a Degludec product
  lmdb_dia[drugclass=="degludec",mindeg:=start,pnr]
  suppressWarnings(lmdb_dia[,mindeg:=min(mindeg,na.rm=T),pnr])
  
  ## Make variable (start_secondline) which holds the min date for either GLP1 or DPP4
  lmdb_dia[,start_secondline:=pmin(mindeg,minglg,na.rm=T),pnr]
  
  #Define arms 
  lmdb_dia[start==start_secondline&drugclass=="degludec",arm:="degludec"]
  lmdb_dia[start==start_secondline&drugclass=="glargine",arm:="glargine"]
  
  #Only include those who are present in one of the arms 
  arms <- lmdb_dia[!is.na(arm),.(pnr)]
  dat <- lmdb_dia[pnr%in%arms$pnr] #take all PNR who are present arms$pnr, ie, they are part of one of the arms
  dat[order(pnr,arm),arm:=na.locf(arm),pnr] #replaces NA with the most recent obs, STOPS when new PNR 
  #dat[,uniqueN(pnr),arm]
  
  #Identify patients who starts both drugs on the same date, we do not include those 
  dat[,numsecond:=uniqueN(arm),pnr]
  ex <- unique(dat[numsecond>1,.(pnr)]) #not that many :-) 
  #ex[,uniqueN(pnr)]
  dat <- dat[!pnr%in%ex$pnr]
  
  return(dat[,.(pnr,start,drugdiab,end,drugclass,index_date=start_secondline,arm)])

  
}