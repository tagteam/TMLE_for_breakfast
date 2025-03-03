map_intervals <- function(grid,data,name,rollforward,values=c(1,0),fill=NA,X.factor=F){
  
  setkey(grid,pnr,date)
  data=data[!is.na(date)]
  
  if(length(data)==0) return(NULL)
  
  setkey(data,pnr,date)
  data[,X:=values[[1]]]
  grid <- data[grid,roll=rollforward]
  #missing value means no event in this interval 
  grid[is.na(grid$X),X:=values[[2]]]
  setkey(grid,pnr,interval)
  wide <- dcast(grid,pnr~interval,value.var="X",sep="_",fill=fill)
  
  #this is for ltmle censored/uncensored
  if(X.factor){
    for(cc in names(wide)[-1]){
      set(wide,j=cc,value=factor(wide[[cc]],levels=values))
    }
  }
  
  grid[,X:=NULL]
  data[,X:=NULL]
  
  setnames(wide,c("pnr",paste0(name,"_",names(wide)[-1])))
  setkey(wide,pnr)
  wide[]
  
}
