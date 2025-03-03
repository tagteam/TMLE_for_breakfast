#tar_load(pnrlist)

fx.get.kom <- function(pnrlist) {
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/Population/',
                         '^bef20(0912|1[0-9]|2[0-3]).*',full.names=TRUE) # bef files last 2014 and then 2015-22
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- read_sas(filelist[x],col_select = c("PNR","REFERENCETID","KOM"))
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  dat <- dat[pnr%in%pnrlist$pnr] #here we filter
  return(dat)
}
