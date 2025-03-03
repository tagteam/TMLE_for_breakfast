fx.baseline.all.LTMLEprep <- function(dt_index,
                                      dt_med,dt_med_dia,
                                      dt_comorb_baseline,
                                      dt_baseline,
                                      regime_data_prep,
                                      dt_deg_glg,
                                      dt_lab) {
  # tar_load(dt_index)
  # tar_load(dt_med)
  # tar_load(dt_med_dia)
  # tar_load(dt_comorb_baseline)
  # tar_load(dt_baseline)
  # tar_load(dt_deg_glg)
  # tar_load(pnrlist_EMPAREG)
  # tar_load(dt_lab)
  # impute_income=T
  
  setnames(dt_deg_glg, c("index_date"), c("index.date"))
  
  impute=T
  dat <- Reduce(function(x,y){merge(x,y,all=TRUE,by="pnr")},
                list(dt_index,dt_med,dt_med_dia,dt_comorb_baseline,dt_baseline))
  
  dat <- dat[pnr%in%regime_data_prep$pnr]
  
  
  #Fix missings in medicine (they are 0, not an imputation) 
  cols <- grep("_180$",names(dat),value=T)
  for(col in cols) {
    set(dat,i=which(is.na(dat[[col]])),j=col,value=0)}
  
  # Fix age & prepare categorization   
  dat[,age:=round(as.numeric((index.date-fdato)/365.24),0)]
  dat[,agegroup:=Publish::acut(age,breaks=c(seq(45,85,by=5),Inf),format="%l-%u",format.low="below %u",format.high="above %l")]
  
  # Impute   
  if(impute==T){
    dat[is.na(income),income:=0]
    dat[is.na(edu),edu:="group1"]
    # dat[is.na(urbanization),urbanization:="suburb"] No urbanization in this data
  }
  
  #Categorize
  dat[,incomegroup:=cut(income,quantile(income,probs=seq(0,1,0.25),na.rm=T),include.lowest=T,
                        labels=c("Q1","Q2","Q3","Q4"))]

  #diabetes duration
  dt_deg_glg <- dt_deg_glg[order(pnr,start),.SD[1],pnr]
  dt_deg_glg[,diabetes_duration:=round((as.numeric(index.date-start))/365.24,0)]
  
  dat <- dt_deg_glg[,.(pnr,diabetes_duration)][dat,on=.(pnr)]
  dat[,diabetes_duration_group:=cut(diabetes_duration,breaks=c(-Inf,1,5,10,Inf),labels=c("< 1","1-5","5-10","> 10"))]
  
  # add hba at baseline 
  hba <- dt_lab[class=="HBA1C"]
  hba <- hba[!is.na(res)&!is.na(start)]
  hba <- hba[,.SD[1],by=.(pnr,start)] #some have more on the same date, take the first...  
  hba[,joindate:=start]
  setnames(hba,c("res","start"),c("hba_res","hba_date"))
  
  setkey(hba,pnr,joindate)
  dat[,joindate:=index.date]
  setkey(dat,pnr,joindate)
  
  dat <- hba[dat,roll=T]
  
  
  dat[,':='(year.x=NULL,year.y=NULL,i.year=NULL,min.start=NULL,joindate=NULL,
            isced_code=NULL,joiny=NULL,kom=NULL,region=NULL,min.start.year=NULL,
            class=NULL,hba_date=NULL,index.date=NULL,arm=NULL,
            fdato=NULL)]
  
  return(unique(dat))
  
  
  
  
}