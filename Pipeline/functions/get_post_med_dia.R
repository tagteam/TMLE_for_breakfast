
fx.post.med.dia <- function(data,lmdb,diamedlist){
  
  # tar_load(dt_index)
  # data=dt_index
  # tar_load(lmdb)
  # tar_load(diamedlist)
  
  excl<-list(ting=c(''))
  
  medicine <- findCondition(lmdb,"atc",keep=c("pnr","eksd","atc"),conditions=diamedlist,exclusions = excl,match='start',condition.name='drug')  
  medicine <- medicine[order(pnr,eksd)]
  
  data[,min.start:=index.date]
  data[,min.start.year:=year(min.start)]
  data <- unique(data[,.(pnr,min.start,min.start.year)])
  
  ##### Define post baseline dia medical therapy
  medicine <- unique(data[,.(pnr,min.start)])[medicine,on=.(pnr)]
  medicine[,is_after:=as.numeric(eksd>min.start)]
  medicine <- medicine[is_after==1]
  
  sd=lapply(unique(medicine$drug),function(d)medicine[drug==d,.(pnr,eksd=eksd,X=as.character(d))])
  names(sd)=unique(medicine$drug)
  
  
  return(sd)
  
}

