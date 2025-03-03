#tar_load(pnrlist)

#Get all medications
fx.get.lmdb <- function(pnrlist) { # Get medications from 2009-2022
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/medication/',
                         '^lmdb20(09|1[0-9]|2[0-2]).*',full.names=TRUE) # lmdb files 2009-2022: obs: co-medication
  cl <- makeCluster(10) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- read_sas(filelist[x],col_select = c("PNR","ATC","eksd","Volume","strnum","PACKSIZE")) 
      #dat <- importSAS(filelist[x],keep=c(("PNR","ATC","eksd","Volume","strnum","PACKSIZE"), obs=1000)
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))
  dat <- dat[pnr%in%pnrlist$pnr]
  return(dat)
}