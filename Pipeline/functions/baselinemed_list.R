baselinemed_list <- function(){
  
  list(
    bb = c('C07'), 
    ccb = c('C08'), 
    rasi = c('C09'), 
    thiazid = c('C03A'), 
    loop = c('C03C', 'C03EB'),
    mra = c('C03D'), 
    digoxin = c('C01AA05'), 
    statin = c('C10A', 'A10BH51', 'A10BH52'), 
    asa = c('B01AC06', 'N02BA01'), 
    adpi = c('B01AC'), 
    vka = c('B01AA'), 
    noac = c('B01AF'), 
    inh_c = c('R03BA', paste0('R03AK0',c(6:9)),paste0('R03AK', c(10:12)), paste0('R03AL0', c(8:9))), 
    inh_b2 = c('R03AC', paste0('R03AK0', c(6:9)), paste0('R03AK', c(10:13)), paste0('R03AL0', c(1:9))), 
    inh_musc = c('R03BB', paste0('R0AL0', c(1:9))), 
    antidep = c('N06A'),
    antipsyc = c('N05A'),
    thyroidea=c("H03"),
    glucocorticoid=c("H02")
  )
}
