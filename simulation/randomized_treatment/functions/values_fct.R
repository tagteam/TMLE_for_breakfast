value_fct <- function(n = c(500,1000), 
                      outcome_prevalence = c(0.1,0.1),
                      death_prevalence = c(0.1),
                      censoring_probability = c(0.2),
                      effects = list(
                        "A_1" = c(0,0,0),
                        "B_1" = c(0,0,0),
                        "L_1" = c(0,0,0),
                        "Y_1" = c(0,0,0),
                        "Censored_1" = c(0,0,0),
                        "Censored_2" = c(0,0,0),
                        "Dead_1" = c(0,0,0),
                        "Y_2" = c(0,0,0,0,0,0))){
  require(foreach)
  foreach(x = names(effects))%do%{
    if(!is.list(effects[[x]])){
      effects[[x]] <- list(effects[[x]])
    } else{
        effects[[x]] <- effects[[x]]
      }
  }
  eval(parse(text = paste0("value = expand.grid(", 
                           paste(unlist(paste0(c("n = ", "outcome_prevalence = ", "death_prevalence = ", "censoring_probability = ",
                                                 "A_1_effects = ", "B_1_effects = ", "L_1_effects = ", "Y_1_effects = ", 
                                                 "Censored_1_effects = ", "Censored_2_effects = ", "Dead_1_effects = ", "Y_2_effects = "), 
                                               list(n, list(outcome_prevalence), death_prevalence, censoring_probability, 
                                                    effects$A_1, effects$B_1, effects$L_1, effects$Y_1,
                                                    effects$Censored_1, effects$Censored_2, effects$Dead_1, effects$Y_2))), collapse = ","),")")))
  value$scenario <- unlist(foreach(x = 1:dim(unique(value[-1]))[1])%do%{rep(paste0("Case_", x), max(1,length(n)))})
  value[]
}

# value_fct <- function(n = NA, treatment_effect = NA, covariate_effect = NA, outcome_prevalence = NA){
#   require(foreach)
#   test <- !is.na(c(n[[1]], treatment_effect[[1]], covariate_effect[[1]], outcome_prevalence[[1]]))
#   eval(parse(text = paste0("value = expand.grid(", 
#                            paste(unlist(ifelse(test,
#                                                paste0(c("n = ", "treatment_effect = ", "covariate_effect = ", "outcome_prevalence = "), 
#                                                       list(n, treatment_effect, list(covariate_effect), outcome_prevalence)), 
#                                                list(NULL))), collapse = ","),")")))
#   value$scenario <- unlist(foreach(x = 1:dim(unique(value[-1]))[1])%do%{rep(paste0("Case_", x), max(1,length(n)))})
#   # apply(value[-1], 1, function(x){paste(paste0(colnames(value[-1]), " = ", x), collapse = ", ")})
#   value[]
# }

