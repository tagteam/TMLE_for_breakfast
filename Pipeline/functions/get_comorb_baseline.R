get_comorb_baseline <- function(data,nyears,dt_comorb_wide){
  
  # tar_load(dt_index)
  # data=dt_index
  # nyears=10
  # tar_load(dt_comorb_wide)
  dt_comorb_wide <- dt_comorb_wide[!is.na(pnr)]
  
  tab <- unique(data[,.(pnr,joindate=index.date,index.date,arm)])
  setkey(tab,pnr,joindate)
  
  vars <- names(dt_comorb_wide)[2:length(dt_comorb_wide)]
  
  # Roll them on 
  pastcomorb <- Reduce(function(x,y){merge(x,y,all.x=T,by="pnr")},
                       (lapply(1:length(vars), function(x){
                         dt_comorb_wide[,joindate:=get(vars[x])]
                         setkey(dt_comorb_wide,pnr,joindate)
                         dat <- dt_comorb_wide[,.(pnr,get(vars[x]),joindate)][tab,roll=365.24*nyears]
                         setnames(dat,"V2",vars[x])
                         return(dat[,1:2])
                       })))
  
  
  for(x in vars) pastcomorb[,(x):=!is.na(get(x))]
  return(pastcomorb)
}