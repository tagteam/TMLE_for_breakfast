fx.get.lmdb.dia <- function(diamedlist) { # Get medications from 2009-2022 Used for defining cohort
  
  
  
  
  
  
  
  filelist <- list.files('X:/Data/Rawdata_Hurtig/703740/Grunddata/medication/',
                         '^lmdb20(0[0-9]|1[0-9]|2[0-3]).*',full.names=TRUE) # lmdb files 2000-2023: obs: co-medication
  cl <- makeCluster(26) # Define cluster with 10 cores
  registerDoParallel(cl)
  dat <- rbindlist(
    foreach(x=1:length(filelist),.packages=c("haven","heaven","data.table")) %dopar% {
      dat <- read_sas(filelist[x],col_select = c("PNR","ATC","eksd","Volume","strnum","PACKSIZE"))
    }
  )
  stopCluster(cl)
  registerDoSEQ()
  gc()
  names(dat) <- tolower(names(dat))

  dat2 <- findCondition(dat,"atc",c("pnr","atc","eksd","volume","packsize"),diamedlist,match="start")
  return(dat2)
}






# 
# {
#   ## fx.get.lmdb.dia(diamedlist=diamedlist),cue=tar_cue(mode=my_mode) #This only generate until 09
#   a10 <- readRDS("z:/Workdata/703740/ThomasAlexanderGerds/dualdiabetes/_targets/objects/A10")
#   out <- findCondition(a10,"atc",c("pnr","atc","eksd"),diamedlist,match="start")
#   out <- setDT(out)
# }
# ),

