# tar_load(dt_sgl_dpp4)

# # Dataset of only pnr and index date and specify study period of interest
# fx.index.population <- function(dt_sgl_dpp4, end_of_followup, start_of_followup) {
#   dt <- unique(dt_sgl_dpp4[,.(pnr,index_date,arm)]) 
#   dt[,year:=year(index_date)]
#   dt <- dt[year>=start_of_followup & year<=end_of_followup] #HERE WE SPECIFY STUDY PERIOD OF INTEREST
#   return(dt)
# }


# Dataset of only pnr and index date and specify study period of interest
fx.index.population <- function(dt_deg_glg) {
  dt <- unique(dt_deg_glg[,.(pnr,index.date=index_date,arm)]) #start_secondline defined by degludec or glargine
  dt[,year:=year(index.date)]
  dt <- dt[year>2011] #HERE WE SPECIFY STUDY PERIOD OF INTEREST
  return(dt)
}