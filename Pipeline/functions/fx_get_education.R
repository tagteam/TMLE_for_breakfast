#tar_load(pnrlist)

fx.get.education <- function(pnrlist) {
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/social/',
                         '^udda20(14|1[5-9]|2[0-3]).*',full.names=TRUE) # education files 2014-2023
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- setDT(read_sas(filelist[x],col_select = c("PNR","HFAUDD")))
      dat[,year:=x+2013]
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  dat <- merge(dat,edu_code,by="hfaudd",all.x=TRUE)
  dat <- dat[pnr%in%pnrlist$pnr]
  return(dat)
  
}