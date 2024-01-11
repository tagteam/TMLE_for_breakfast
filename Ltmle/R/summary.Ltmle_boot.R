get_cheap_subsampling_ci_t_test <- function(est, boot_est, m_val,n_val,B,mu,alpha){
  B <- length(boot_est)
  S <-sqrt(mean((est-boot_est)^2))
  tq <- qt(1-alpha/2,df = B)
  T_stat <- (est-mu)/(sqrt((m_val)/(n_val-m_val)) * S)
  list(lower_b = est - tq * sqrt((m_val)/(n_val-m_val)) * S,
       upper_b = est + tq * sqrt((m_val)/(n_val-m_val)) * S,
       T_stat = T_stat,
       df = B,
       pvalue_b = 2*(1-pt(abs(T_stat),df=B)))
}

get_monte_carlo_se_subsampling_ci <- function(est, boot_est,m_val,n_val,alpha){
  se <- sd(boot_est)*sqrt(m_val/(n_val-m_val))
  list(lower_b = est-qnorm(1-alpha/2)*se,
       upper_b = est+qnorm(1-alpha/2)*se,
       T_stat = NA,
       df = NA,
       pvalue_b = NA)
}

# Theorem 2.5.2, Politis states that the following CI has valid coverage for any distribution of the data
get_percentile_subsampling_ci <- function(est, boot_est,m_val,n_val,alpha){
  percentile_normed <- quantile(sqrt(m_val)*(boot_est-est),probs = c(alpha/2,1-alpha/2))/sqrt(n_val)
  list(lower_b = est-percentile_normed[2],
       upper_b = est-percentile_normed[1],
       T_stat = NA,
       df = NA,
       pvalue_b = NA)
}

summary.Ltmle_boot <- function(object,...,truncate=TRUE,subsample_method="cheap",conf_level = 0.95) {
  out <- summary.Ltmle(object$result, ...)
  B <- length(object$res_boot)
  out_b <- rbindlist(lapply(1:B,function(b) summary.Ltmle(object$res_boot[[b]], ...)))[,c(1:3)]
  setnames(out_b,"estimate","estimate_b")
  out_b <- merge(out,out_b)
  alpha <- 1-conf_level
  if (subsample_method=="cheap"){
    out_b <- out_b[,(get_cheap_subsampling_ci_t_test(estimate[1],estimate_b,object$m,object$n,B,0,alpha)),
                   by = list(Target_parameter,Estimator)]
  } else if (subsample_method=="monte_carlo_se"){
    out_b <- out_b[,(get_monte_carlo_se_subsampling_ci(estimate[1],estimate_b,object$m,object$n,alpha)),
                   by = list(Target_parameter,Estimator)][,c(1,2,3,4)]
  } else if (subsample_method=="percentile"){
    out_b <- out_b[,(get_percentile_subsampling_ci(estimate[1],estimate_b,object$m,object$n,alpha)),
                   by = list(Target_parameter,Estimator)][,c(1,2,3,4)]
  } else {
    stop("subsample_method must be either cheap or monte_carlo_se")
  }
  ## for rows which contain the target parameter ATE, if lower_b is lower than -1, set it to -1 and if upper_b is higher than 1, set it to 1
  if (truncate){
    out_b[grepl("ATE",Target_parameter),lower_b:=pmax(lower_b,-1)]
    out_b[grepl("ATE",Target_parameter),upper_b:=pmin(upper_b,1)]
    out_b[grepl("Mean",Target_parameter),lower_b:=pmax(lower_b,0)]
    out_b[grepl("Mean",Target_parameter),upper_b:=pmin(upper_b,1)]
    out_b[grepl("Ratio",Target_parameter),lower_b:=pmax(lower_b,0)]
  }
  out <- merge(out,out_b)
  out
}

## get cheap subsampling cis and test for multiple values of B
## Note: inefficient implementation, but we won't use that many B values anyway
multiple_summary_cs <- function(object,..., Bs=seq_along(object$res_boot)){
  out <- list()
  for (b in Bs){
    temp <- object
    temp$res_boot <- temp$res_boot[1:b]
    out[[b]]  <- summary.Ltmle_boot(temp,...)
    out[[b]]$B <- b
  }
  out <- rbindlist(out)
  out
}

plot_cs_convergence <- function(object,..., subsample_methods=c("cheap","monte_carlo_se","percentile"),Bs=seq_along(object$res_boot)){
  require(ggplot2)
  require(gridExtra)
  require(data.table)
  out <- list()
  if ("cheap" %in% subsample_methods){
    object_cheap <- multiple_summary_cs(object,...,Bs=Bs,subsample_method="cheap")
    ## set T_stat, df and pvalue_b from object_cheap to null
    object_cheap[,c("T_stat","df","pvalue_b"):=NULL]
    object_cheap[,subsample_method:="cheap"]
    out[["cheap"]] <- object_cheap
  }
  if ("monte_carlo_se" %in% subsample_methods){
    object_monte_carlo_se <- multiple_summary_cs(object,...,Bs=Bs,subsample_method="monte_carlo_se")
    object_monte_carlo_se[,subsample_method:="monte_carlo_se"]
    out[["monte_carlo_se"]] <- object_monte_carlo_se
  }
  if ("percentile" %in% subsample_methods){
    object_percentile <- multiple_summary_cs(object,...,Bs=Bs,subsample_method="percentile")
    object_percentile[,subsample_method:= "percentile"]
    out[["percentile"]] <- object_percentile
  }
  out <- rbindlist(out)
  if (length(subsample_methods) > 1){
    ## set subsampling_method as factor
    out[,subsample_method:=factor(subsample_method)]
    a<-ggplot(out,aes(x=B,y=lower_b,color = subsample_method))+geom_point()+facet_wrap(.~Target_parameter,scales="free_y")+geom_hline(aes(yintercept=lower))+ylab("")+ggtitle("Lower endpoint of subsampling confidence interval against the one from variance.method")
    b<-ggplot(out,aes(x=B,y=upper_b,color = subsample_method))+geom_point()+facet_wrap(.~Target_parameter,scales="free_y")+geom_hline(aes(yintercept=upper))+ylab("")+ggtitle("Upper endpoint of subsampling confidence interval against the one from variance.method")
    ## change subsample_method in a to be appear as "Subsample method"
    a <- a + scale_color_discrete(name = "Subsample method")
    ## change subsample_method in b to be appear as "Subsample method"
    b <- b + scale_color_discrete(name = "Subsample method")
  }
  else {
    ## set out to object_cheap if subsample_method is cheap and to object_monte_carlo_se if subsample_method is monte_carlo_se
    a<-ggplot(out,aes(x=B,y=lower_b))+geom_point()+facet_wrap(.~Target_parameter,scales="free_y")+geom_hline(aes(yintercept=lower))+ylab("")+ggtitle("Lower endpoint of subsampling confidence interval against the one from variance.method")
    b<-ggplot(out,aes(x=B,y=upper_b))+geom_point()+facet_wrap(.~Target_parameter,scales="free_y")+geom_hline(aes(yintercept=upper))+ylab("")+ggtitle("Upper endpoint of subsampling confidence interval against the one from variance.method")
  }
  grid.arrange(a,b,ncol=2)
}