get_baseline_med <- function(data,lmdb,medlist){
  # tar_load(dt_index)
  # data=dt_index
  # tar_load(lmdb)
  # tar_load(medlist)


  excl<-list(ting=c(''))
  
  medicine <- findCondition(lmdb,"atc",keep=c("pnr","eksd","atc"),conditions=medlist,exclusions = excl,match='start',condition.name='cond')  
  medicine <- medicine[order(pnr,eksd)]
  
  data[,min.start:=index.date]
  data[,min.start.year:=year(min.start)]
  data <- unique(data[,.(pnr,min.start,min.start.year)])
  
  
  ##### Define baseline medical therapy
  medicine <- unique(data[,.(pnr,min.start)])[medicine,on=.(pnr)]
  medicine[,is_before:=as.numeric(eksd<min.start)]
  medicine <- medicine[is_before==1,.(max=max(eksd)),by=.(pnr,cond)]
  medicine <- unique(data[,.(pnr,min.start)])[medicine,on=.(pnr)]
  
  medicine1 <-dcast(medicine,pnr+min.start~cond,value.var = 'max')  
  
  ## complete binary indicator of treatments 6 months previously
  search_list<-names(medlist)
  
  med_180 <- medicine1[, paste0(search_list,'_180'):=(lapply(.SD,function(x){
    ifelse(!is.na(x) & ((difftime(as.Date(min.start),x,units=c('days')))<=180) & ((difftime(min.start,x,units=c('days')))>=1),1,0)
  })),.SDcols=search_list]
  cols=med_180[,c("pnr",grep("_180",names(med_180),value=TRUE))]
  med_180 <- med_180[,..cols]
  
  return(med_180)
  
}



get_baseline_med_dia <- function(data,lmdb,diamedlist){
  # tar_load(dt_sgl_dpp4)
  # data=dt_index
  # tar_load(lmdb)
  # tar_load(diamedlist)
  
  excl<-list(ting=c(''))
  
  medicine <- findCondition(lmdb,"atc",keep=c("pnr","eksd","atc"),conditions=diamedlist,exclusions = excl,match='start',condition.name='cond')  
  medicine <- medicine[order(pnr,eksd)]
  
  data[,min.start:=index.date]
  data[,min.start.year:=year(min.start)]
  data <- unique(data[,.(pnr,min.start,min.start.year)])
  
  ##### Define baseline medical therapy
  medicine <- unique(data[,.(pnr,min.start)])[medicine,on=.(pnr)]
  medicine[,is_before:=as.numeric(eksd<min.start)]
  medicine <- medicine[is_before==1,.(max=max(eksd)),by=.(pnr,cond)]
  medicine <- unique(data[,.(pnr,min.start)])[medicine,on=.(pnr)]
  
  medicine1 <-dcast(medicine,pnr+min.start~cond,value.var = 'max')  
  
  ## complete binary indicator of treatments 6 months previously
  search_list<-names(medicine1)[3:length(medicine1)]#length(medicine1)
  
  # <-- FIX THIS TO BE ONLY THOSE FOUND IN MEDICINE
  
  med_180 <- medicine1[, paste0(search_list,'_180'):=(lapply(.SD,function(x){
    ifelse(!is.na(x) & ((difftime(as.Date(min.start),x,units=c('days')))<=180) & ((difftime(min.start,x,units=c('days')))>=1),1,0)
  })),.SDcols=search_list]
  
  cols=med_180[,c("pnr",grep("_180",names(med_180),value=TRUE))]
  med_180 <- med_180[,..cols]
  
 
  
  
  
  return(med_180)
  
}




