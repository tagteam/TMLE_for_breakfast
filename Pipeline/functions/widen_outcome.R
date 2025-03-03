widen_outcome <- function(outcome,data,intervals,como){
  # tar_load(regime_data_prep)
  # data=regime_data
  
  data <- data[,.SD[1],pnr]
  
  data[,slut:=pmin(doddato,emi.date,as.Date("2022-12-31"),na.rm=T)]
  length_interval=unique(round(diff(intervals),0))
  stopifnot(length(length_interval)==1)
  
  #TAG VERSION:
  #one line per interval 
  grid <- data[,.(date=start+intervals,slut),by=pnr] 
  grid[,interval:=0:(length(intervals)-1),by=pnr]
  
  grid <- grid[date<=slut+length_interval]
  wide=map_intervals(grid=grid,data=data[!is.na(doddato),list(pnr=pnr,date=doddato)],name="Dead",rollforwar=Inf)
  #naturally, NA values means censored, hence fill=1
  w=map_intervals(grid=grid,data=data[is.na(doddato),list(pnr=pnr,date=slut)],name="Censored",rollforward=Inf,values=c("censored","uncensored"),fill="censored",X.factor=TRUE)
  wide=wide[w] #combine
  
  #outcome ----
  
  if (length(outcome[[1]])==1){
    outcome <- outcome[[1]]
    outcome_data=como[[outcome]][,list(pnr=pnr,date=inddto,discharge=uddto)]
  }else{
    outcome_data <- rbindlist(lapply(outcome[[1]],function(oo){
      como[[oo]][,list(pnr,date=inddto,discharge=uddto)]
    }))
    outcome=names(outcome)
  }
  setkey(outcome_data,pnr,date)
  print(outcome)
  setkey(outcome_data,pnr)
  setkey(data,pnr)
  outcome_data=data[,.(pnr,start)][outcome_data] #combine
  
  # only interested in new outcomes, but want to tag patients who are in hospital at baseline
  
  ## hospital diagnoses overlapping start
  admitted_index=outcome_data[date>=start & discharge>start,unique(pnr)]
  outcome_data=outcome_data[date>start]
  ## only interested in first new outcome 
  setkey(outcome_data,pnr,date)
  outcome_data=outcome_data[outcome_data[,.I[1],by=c("pnr")]$V1]
  w=map_intervals(grid=grid,data=outcome_data,name=outcome,rollforward=Inf)
  # when outcome occurs at baseline re-set to value 2
  # set(w,i=which(wide$pnr%in%admitted_index),j=paste0(outcome,"_0"),value=2)
  wide=wide[w]
  
  # Some notes: 
  ## 1) outcome is carried forward
  ## 2) outcome in interval which starts at index does not count as outcome 
  ## 3) patients who die at index are excluded 
  ## 4) when outcome occurs before death in interval, death date does not matter
  ## 5) when censoring occurs, but not outcome, outcome=NA
  
  for (tk in 1:(length(intervals)-2)){
    Ok=paste0(outcome,"_",tk)
    Ok_next=paste0(outcome,"_",tk+1)
    Dk=paste0("Dead","_",tk)
    Dk_next=paste0("Dead","_",tk+1)
    Ck=paste0("Censored","_",tk)
    Ck_next=paste0("Censored","_",tk+1)
    #when died or outcome at t_k then also at t_{k+1}
    if(any(has_died <- wide[[Dk]]==1))
      set(wide,j=Dk_next,i=which(has_died),value=1)
    if(any(has_outcome <- wide[[Ok]]==1))
      set(wide,j=Ok_next,i=which(has_outcome),value=1)
    
    #last outcome carried forward
    if(any(miss_outcome <- is.na(wide[[Ok_next]])))
      set(wide,j=Ok_next,i=which(miss_outcome),value=wide[miss_outcome][[Ok]])
    # when censored in interval but not outcome, then outcome and death are both NA
    has_censored <- wide[[Ck]]==1
    if(any(has_censored & !(has_outcome)))
      set(wide,j=Ok_next,i=which(has_censored & !(has_outcome)),value=NA)
    if(any(has_censored & !(has_died)))
      set(wide,j=Dk_next,i=which(has_censored & !(has_died)),value=NA)

  }
  wide 
}
