fx.widen.regimens.LTMLEprep <- function(regime_data_prep,add_B_nodes=FALSE,
                                    intervals=seq(0,10*6*30.45,6*30.45)){
  #tar_load(regime_data_prep)
  #intervals=seq(0,10*6*30.45,6*30.45)
  #add emigration and death

  regime_base_data <- unique(regime_data_prep[start>=index_date,.(pnr,start,emi.date,end,doddato,index_date,arm,drugclass,drugdiab)])
  regime_base_data <- regime_base_data[order(pnr,start),.SD[1],by=pnr]
  print(dim(regime_base_data))
  regime_base_data[,slut:=pmin(doddato,emi.date,as.Date("2022-12-31"),na.rm=TRUE)]

  grid <- regime_base_data[,.(date=start+intervals),by=pnr]
  grid[,interval:=0:(length(intervals)-1),by=pnr]
  grid <- regime_base_data[,.(pnr,slut)][grid,on=.(pnr)]
  # in the last interval the death date is > than the
  # start of the interval, hence we keep the last interval
  # in for now
  length_interval=unique(round(diff(intervals),0)) # 183 days
  grid <- grid[date<=slut+length_interval]

  merge_intervals <- function(grid,data,name,rollforward){
    grid=copy(grid)
    setkey(grid,pnr,date)
    setkey(data,pnr,date)
    data[,X:=1]
    grid <- data[grid,roll=rollforward]
    grid[is.na(grid$X),X:=0]
    setkey(grid,pnr,interval)
    grid[]
  }

  setkey(grid,pnr,date)
  x=copy(grid)

  regime_data_prep[,drugclass:=as.factor(drugclass)]
  # check drug prescriptions in personalized intervals
  for (d in levels(regime_data_prep$drugclass)){
    print(d)
    drug_data=regime_data_prep[drugclass==d][,drugclass:=NULL]
    setnames(drug_data,"start","date")
    ## w=merge_intervals(grid=grid,data=drug_data,name=d,rollforward=6*30)
    w=merge_intervals(grid=grid,data=drug_data,name=d,rollforward=(length_interval - 1))
    # We subtract 1 to get half-closed intervals incl. left endpoint
    # this way no observation will count for two intervals
    setnames(w,"X",d)
    setkey(w,pnr,date)
    x=x[w[,.SD,.SDcols=c("pnr","date",d)]]
    x[]
  }
    x<- x[!(interval==0&duplicated(pnr))]

  out=data.table::dcast(x,
                        pnr~interval,
                        value.var=c(levels(regime_data_prep$drugclass)))
  
  out2 <- list(degludec=cbind(out[,.(pnr)],out[,grep("degludec",names(out)),with=F]),glargine=cbind(out[,.(pnr)],out[,grep("glargine",names(out)),with=F]))
  
  
  
  
  if(add_B_nodes==TRUE){
    out2 <- list(
      degludec=cbind(out[,.(pnr)],
                  out[,grep("^degludec",names(out)),with=F],
                  setnames(out[,grep("^glargine",names(out)),with=F],paste0("glargine_",0:10),paste0("B_",0:10))),
      glargine=cbind(out[,.(pnr)],
                 out[,grep("^glargine",names(out)),with=F],
                 setnames(out[,grep("^degludec",names(out)),with=F],paste0("degludec_",0:10),paste0("B_",0:10))))
  } else {
    
    out2 <- list(degludec=cbind(out[,.(pnr)],out[,grep("^degludec",names(out)),with=F]),
                 glargine=cbind(out[,.(pnr)],out[,grep("^glargine",names(out)),with=F]))
  }
  
  
  

  return(out2)
  
}





