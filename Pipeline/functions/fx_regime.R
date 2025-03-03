fx.regime_prep_block <- function(exclusions_DEVOTE, immiemi, death, dt_deg_glg) {
  dt <- dt_deg_glg[pnr%in%exclusions_DEVOTE_block$pnr]
  dt <- death[, .(pnr, doddato)][dt, on = .(pnr) ]
  ud <- unique(immiemi[indud_kode=="U",.(pnr,emi.date=haend_dato,joindate=haend_dato)])
  setkey(ud,pnr,joindate)
  dt[,joindate:=index_date]
  setkey(dt,pnr,joindate)
  dt <- ud[dt,roll=-Inf] #roll forward
  dt <- dt[start>=index_date]
  dt[,':='(joindate=NULL)]
  return(dt)
}



fx.regime_prep <- function(exclusions_DEVOTE, immiemi, death, dt_deg_glg,end_of_data,end_of_followup) {
  dt <- dt_deg_glg[pnr%in%exclusions_DEVOTE$pnr]
  dt <- death[, .(pnr, doddato)][dt, on = .(pnr) ]
  ud <- unique(immiemi[indud_kode=="U",.(pnr,emi.date=haend_dato,joindate=haend_dato)])
  setkey(ud,pnr,joindate)
  dt[,joindate:=index_date]
  setkey(dt,pnr,joindate)
  dt <- ud[dt,roll=-Inf] #roll forward
  dt <- dt[start>=index_date]
  dt[,':='(joindate=NULL)]
  dt <- dt[start<end_of_followup]
  dt[,slut:=pmin(doddato,emi.date,end_of_data,end_of_followup,na.rm=TRUE)]
  return(dt)
}