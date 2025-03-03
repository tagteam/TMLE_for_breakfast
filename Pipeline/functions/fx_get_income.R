#tar_load(pnrlist)


fx.get.income <- function(pnrlist) {
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/social/',
                         '^ind20(08|09|1[0-9]|2[0-3]).*',full.names=TRUE) # income files 2008-2022
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- setDT(read_sas(filelist[x],col_select = c("PNR","AEKVIVADISP_13")))
      dat[,year:=x+2007]
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  dat <- dat[pnr%in%pnrlist$pnr]
  return(dat)
  
}