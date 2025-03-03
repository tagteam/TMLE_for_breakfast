comorb_list <- function(){
  
  list( 
    cvd = c(as.character(390:459), paste0("DI0", c(0:9)), paste0("DI", c(1:9))), 
    mi = c("410","DI200", "DI21","DI22"), #<-- DOUBLE TJEK THIS 
    ihd=c("411","412","413","414","DI20","DI23","DI24","DI25"),
    uap = c("411", "DI20"), #unstable angina
    stroke = c(as.character(430:434), "DI60", "DI61", "DI63", "DI64", "DG45"),
    tia = c("DG45"),
    heart.failure = c("425", "428", "42709","42710", "42711", "DI110", "DI130", "DI132", "I420", paste0("DI42", c(6:9)), "DI50"), 
    peripheral.vascular.disease = c("443", "DI70", "DI739"),
    renal.disease = c('25002', '40039', c(403:404),c(581:584), '59009', '75310', '75311', '75319' ,'DN02','DN03','DN04','DN05','DN06','DN07','DN08','DN11','DN12','DN14','DN17', 'DN18','DN19','DN26', 
                      'DN158', 'DN159', 'DN160' ,'DN162' ,'DN163', 'DN164', 'DN168' ,'DQ612' ,'DQ613', 'DQ615', 'DQ619','DE112' ,'DE132', 'DE142', 'DI120', 'DR34'), 
    hypertension=c(paste0('40',1:4),'41009','41099','DI10','DI109','DI11','DI110','DI119','DI119A',         
                   'DI12','DI120','DI129','DI129A','DI13','DI130','DI131','DI132','DI139','DI15','DI150','DI151','DI152','DI158','DI159'), 
    diab_nefropathy = c("58381","25002", "DN083", "DE102", "DE112", "DE122", "DE132", "DE142" ), 
    diab_neuropathy = c("25003", "DE104", "DE114", "DE124", "DE134", "DE144"), 
    diab_retinopathy = c("25001", "DE113", "DE123", "DE133", "DE143"),
    afib = c("42793", "DI48"),
    copd = c('490','491','492', 'DJ42', 'DJ44'),  #490-492 ICD 8 
    cancer = c(c(140:209), paste0("DC", c(0:9))), 
    dialysis = c("DZ99"),
    txr = c("DZ940"),
    Hypogly = c("DE159","DE159B","DE160","DE161B","DE161","DE162"),
    depression= c("29609","29629","29809", "30019", "30049", "DF32", "DF33", "DF34", "DF38", "DF39"),
    othermental = c('295', '29619', '29639', c(29689:29700), c(28919:29899), "DF20", "DF21", "DF22", "DF23", "DF24", "DF25", "DF28", "DF29", "DF31")
    
  )
  
  
}