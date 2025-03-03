#Make comorbs wide

#tar_load(dt_comorb)

fx.make.comorb.wide <- function(dt_comorb) {
  #to maintain hierarchi, it wont work without it
  dt_comorb <- rbindlist(lapply(1:length(dt_comorb),function(x)dt_comorb[[x]]))
  dt_comorb[,uddto:=NULL]
  dt_comorb[,id:=1:.N,.(pnr,X)]
  dt_comorb <- dcast(dt_comorb,pnr+id~X,value.var = "inddto")
  dt_comorb[,id:=NULL]
  return(dt_comorb)
}
