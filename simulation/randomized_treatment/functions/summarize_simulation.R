summarize_simulation <- function(simulated_estimator, dgm_target_parameter){
  require(foreach)
    res <- foreach(group = unique(simulated_estimator$tar_group), .combine = "rbind")%do%{
      sim <- copy(simulated_estimator)[tar_group == group][,.(Estimate, SD, lower, upper, n, scenario)]
      case <- unique(sim$scenario)
      True <- copy(dgm_target_parameter)[scenario == case][,Risk]
      data <- data.table(N=sim[,.N])
      data <- cbind(True, sim, data)
      data[,Estimate_mean:=mean(Estimate)]
      data[,Bias:=mean(Estimate - True)]
      data[,Abs_bias:=mean(abs(Estimate - True))]
      data[,Relative_bias:=mean(Estimate - True)/True]
      data[,MSE:=mean((Estimate - True)^2)]
      data[,Coverage:=mean(between(True, lower, upper))]
      data[,SE:=sd(Estimate)]
      data[,meanSD:=mean(SD)]
  
      data[,.(scenario, n, N, True, Estimate_mean, Bias, Abs_bias, Relative_bias, MSE, Coverage, meanSD, SE)][1,]
    }
    res[]
}



# res <- foreach(EST = unique(simulated_estimator$Estimate), .combine = "rbind")%do%{
#   tmp_cases <- foreach(group = unique(simulated_estimator$tar_group), .combine = "rbind")%do%{
#     sim <- copy(simulated_estimator)[Estimate == EST & tar_group == group]
#     case <- unique(sim$scenario)
#     true <- copy(dgm_target_parameter)[scenario == case]
#     data <- data.table(Estimate=EST, N=sim[,.N])
#     data <- cbind(data, sim[,c(which(colnames(sim)%in%c("n", "scenario", "treatment_effect", "covariate_effect", "outcome_prevalence"))), 
#                             with = FALSE][1,])
#     foreach(x = c("Risk_treated", "Risk_untreated", "ATE"))%do%{
#       data[,paste0("Bias_",x):=mean(sim[[x]] - true[[x]])]
#       data[,paste0("Abs_bias_",x):=mean(abs(sim[[x]] - true[[x]]))]
#       data[,paste0("Relative_bias_",x):=mean(sim[[x]] - true[[x]])/true[[x]]]
#       data[,paste0("MSE_",x):=mean((sim[[x]] - true[[x]])^2)]
#       data[,paste0("Coverage_",x):=mean(between(true[[x]], sim[[paste0("lower_", x)]], sim[[paste0("upper_", x)]]))]
#       data[,paste0("SE_",x):=sd(sim[[x]])]
#       data[,paste0("SD_",x):=mean(sim[[paste0("SE_",x)]])]
#     }
#     data[]
#   }
#   tmp_cases[]
# }
# res[]