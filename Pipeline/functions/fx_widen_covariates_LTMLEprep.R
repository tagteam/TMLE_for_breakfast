fx.widen.covar.LTMLEprep <- function(
                             regime_data_prep,
                             intervals,
                             dt_med_dia_post,
                             dt_comorb,
                             exclusions_DEVOTE){
  
  
  
  
  # tar_load(regime_data)
  data=regime_data_prep
  # intervals=seq(0,10*6*30.45,6*30.45)
  # tar_load(dt_med_dia_post)
  med=dt_med_dia_post
  # tar_load(dt_comorb)
  como=dt_comorb
  
  data <- data[pnr%in%exclusions_DEVOTE$pnr]
  
  data <- data[,.SD[1],pnr]
  data[,slut:=pmin(doddato,emi.date,as.Date("2022-12-31"),na.rm=T)]
  grid <- data[,.(date=start+intervals),by=.(pnr)]
  grid[,interval:=0:(length(intervals)-1),by=.(pnr)]
  setkey(grid,pnr)
  setkey(data,pnr)
  grid <- data[,.(pnr,slut)][grid]
  
  #comments about intervals and death.. 
  length_interval=unique(round(diff(intervals),0))
  grid <- grid[date<=slut+length_interval]
  
  merge_intervals <- function(grid,data,name,rollforward){
    grid=copy(grid)
    setkey(grid,pnr,date)
    setkey(data,pnr,date)
    #remove dubs
    data=data[,.SD[1],.(pnr,date)]
    data[,X:=1]
    grid <- data[grid,roll=rollforward]
    grid[is.na(grid$X),X:=0]
    grid[,X:=as.numeric(X)]
    setkey(grid,pnr,interval)
    grid[]
  
  }
  
  
  setkey(grid,pnr,date)
  x=copy(grid)
  covariates=c(c("ihd","heart.failure","stroke","peripheral.vascular.disease",
                 "renal.disease","copd","hypertension","afib"),
               c("insulin_aspart","dpp4_inhib","sglt2_inhib","sulfonylurea","metformin","dulagltd","liraglutd","otherglp","semagltd")) 
  
  if (length(names(como))>0)
    for (com in intersect(names(como),covariates)){
      print(com)
      drug_data=como[[com]][,.(pnr,date=inddto,X)]
      w=merge_intervals(grid=grid,data=drug_data,name=com,rollforward=10*365.25)
      setkey(w,pnr,date) #chronic
      w[,X:=cummax(X),pnr] #ensure that the conditions remain chronic, new 08Jan2025
      setnames(w,"X",com) #chronic
      # setnames(w,"X",com) #nonchronic
      # setkey(w,pnr,date) #nonchronic
      x=x[w[,.SD,.SDcols=c("pnr","date",com)]]
      x[]
    }
  
  #check drug prescriptions in personalized intervals 
  if (length(names(med))>0)
    for (m in intersect(names(med),covariates)){
      print(m)
      drug_data=med[[m]][,.(pnr,date=eksd,X)]
      w=merge_intervals(grid=grid,data=drug_data,name=m,rollforward=length_interval-1)
      setnames(w,"X",m)
      setkey(w,pnr,date)
      x=x[w[,.SD,.SDcols=c("pnr","date",m)]]
      x[]
    }
  
  out=data.table::dcast(x,
                        pnr~interval,
                        value.var=c(intersect(covariates,names(med)),
                                    intersect(covariates,names(como))))
  

#Add the GLP1 together  

for (y in 0:10){
  out[,paste0("glp1_",y):=fifelse(rowSums(.SD ==1, na.rm=TRUE)>0,1,
                                  fifelse(rowSums(.SD ==0, na.rm=TRUE)>0,0,NA_real_)),
      .SDcols=c(paste0("liraglutd_",y),paste0("otherglp_",y),paste0("semagltd_",y),paste0("dulagltd_",y))]
  out[,c(paste0("liraglutd_",y),paste0("otherglp_",y),paste0("semagltd_",y),paste0("dulagltd_",y)):=NULL]
}

return(out)
  
  
}
