## Adjust data to fit ltmle constraints
get_ltmle_data <- function(work_data, time_horizon,
                           name_outcome,
                           name_baseline_covariates,
                           name_time_covariates,
                           name_regimen,
                           name_censoring,
                           censored_label,
                           name_competing_risk,
                           abar){
  time_horizon = max(time_horizon)
  time_grid = 0:time_horizon
  K = length(time_grid)
  if(length(name_censoring)>0){
    for(col in sapply(time_grid[-1], function(timepoint){paste0(name_censoring,"_",timepoint)})){
      set(work_data, j = col, value=as.factor(ifelse(work_data[[col]]%in%censored_label,"censored","uncensored")))
    }
  }
  
  ## Manipulation of the event nodes
  A_nodes = unlist(lapply(time_grid[-K], function(time){paste0(name_regimen, "_", time)}))
  Y_nodes = unlist(lapply(time_grid[-1], function(time){paste0(name_outcome, "_", time)}))
  D_nodes = unlist(lapply(time_grid[-c(1,K)], function(time){paste0(name_competing_risk, "_", time)}))
  C_nodes = unlist(lapply(time_grid[-1], function(time){paste0(name_censoring, "_", time)}))
  A_nodes_position = match(A_nodes, names(work_data))
  Y_nodes_position = match(Y_nodes, names(work_data))
  D_nodes_position = match(D_nodes, names(work_data))
  C_nodes_position = match(C_nodes, names(work_data))

  ## Adjust data depending on censoring/event/competing event with NA
  
  for(q in 1:(K-1)){
    if(q<(K-1)){
      has_outcome_or_death_and_censored = (((work_data[[Y_nodes_position[[q]]]]%in%"1")|(work_data[[D_nodes_position[[q]]]]%in%"1"))&
                                             (work_data[[C_nodes_position[[q]]]]%in%"censored"))
    } else{
      has_outcome_or_death_and_censored = ((work_data[[Y_nodes_position[[q]]]]%in%1)&(work_data[[C_nodes_position[[q]]]]%in%"censored"))
    }
    if(any(has_outcome_or_death_and_censored)){
      set(work_data,j=C_nodes_position[[q]],i=which(has_outcome_or_death_and_censored),value="uncensored")
    }
  }
  
  ## All nodes (except outcome and competing risk) should be NA after an event (outcome or death)
  if(time_horizon!= 1){
      for(k in Y_nodes_position[-(K-1)]){
          later_nodes=setdiff((k+1):NCOL(work_data),Y_nodes_position)
          later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
          # later_C_nodes=intersect((k+1):NCOL(work_data),C_nodes_position)
          # later_D_nodes=intersect((k+1):NCOL(work_data),D_nodes_position)
          if(any(has_outcome <- (work_data[[k]]%in%1))){
              for(l in later_nodes) {set(work_data,j=l,i=which(has_outcome),value=NA)}
              for(l in later_Y_nodes) {set(work_data,j=l,i=which(has_outcome),value=1)}
              # for(l in later_C_nodes) {set(work_data,j=l,i=which(has_outcome),value="uncensored")}
              # for(l in later_D_nodes) {set(work_data,j=l,i=which(has_outcome),value=0)}
          }
      }
      if(length(name_competing_risk)>0){
          for(k in D_nodes_position){
              later_nodes=setdiff((k+1):NCOL(work_data),Y_nodes_position)
              # Later outcome event nodes are set to 0
              later_Y_nodes=intersect((k+1):NCOL(work_data),Y_nodes_position)
              # later_C_nodes=intersect((k+1):NCOL(work_data),C_nodes_position)
              # later_D_nodes=intersect((k+1):NCOL(work_data),D_nodes_position)
              if(any(has_died <- (work_data[[k]]%in%1))){
                  for(l in later_nodes) {set(work_data,j=l,i=which(has_died),value=NA)}
                  for(l in later_Y_nodes) {set(work_data,j=l,i=which(has_died),value=0)}
                  # for(l in later_C_nodes) {set(work_data,j=l,i=which(has_died),value="uncensored")}
                  # for(l in later_D_nodes) {set(work_data,j=l,i=which(has_died),value=1)}
              }
          }
      }
      ## All nodes should be NA as soon as censoring has occurred
      if(length(name_censoring)>0){
          for(k in C_nodes_position){
              later_nodes=(k+1):NCOL(work_data)
              if(any(has_censored <- (work_data[[k]]%in%"censored"))){
                  for(l in later_nodes) {set(work_data,j=l,i=which(has_censored),value=NA)}
              }
          }
      }
  }else{
      # 
      # time_horizon = 1 we set the outcome to NA in case of censored
      #
      if(length(name_censoring)>0){
          for(k in C_nodes_position){
              later_nodes=(k+1):NCOL(work_data)
              if(any(has_censored <- (work_data[[k]]%in%"censored"))){
                  for(l in later_nodes) {set(work_data,j=l,i=which(has_censored),value=NA)}
              }
          }
      }
  }
  # Data at risk and events during followup
  N=NROW(work_data)
  E=0
  CR=0
  C=0
  if (is.list(abar)){
      ## warning("Counting regimen adherence only for first element of abar")
      REG=sum(work_data[[A_nodes_position[[1]]]]==abar[[1]][[1]])
  }else{
      # if A,B then B_0 is obsolete because A0=1-B0
      REG=work_data[[A_nodes_position[[1]]]]==abar[[1]]
  }
  event_counts <- data.table(time=0:time_horizon,
                             atrisk=c(N,rep(NA,time_horizon)),
                             nevent=c(E,rep(NA,time_horizon)),
                             ncomprisk=c(CR,rep(NA,time_horizon)),
                             ncensored=c(C,rep(NA,time_horizon)),
                             nregimen=c(sum(REG),rep(NA,time_horizon)))
  for (s in 1:time_horizon){
      if (s<time_horizon){
          if (is.list(abar)){
              REG <- REG & (work_data[[A_nodes_position[[s+1]]]]%in%abar[[1]][[s+1]])
          }else{
              if (length(name_regimen)==1)
                  REG <- REG & (work_data[[A_nodes_position[[s+1]]]]%in%abar[[s+1]])
              else
                  REG <- REG & ((work_data[[A_nodes_position[[s*2]]]]%in%abar[[s*2]])
                      &
                      (work_data[[A_nodes_position[[s*2+1]]]]%in%abar[[s*2+1]]))
          }
      }
      E=sum(work_data[[Y_nodes_position[[s]]]]%in%1)
      if (length(D_nodes_position)>0)
          if (s<time_horizon)
              CR=sum(work_data[[D_nodes_position[[s]]]]%in%1)
          else
              CR=NA
      C=sum(work_data[[C_nodes_position[[s]]]]%in%"censored")
      if (s<time_horizon)
          event_counts[time==s,nregimen:=sum(REG)]
      else
          event_counts[time==s,nregimen:=NA]
      event_counts[time==s,nevent:=E]
      event_counts[time==s,ncomprisk:=CR]
      event_counts[time==s,ncensored:=C]
      N=N-E-CR-C
      event_counts[time==s,atrisk:=N]
  }
  # L_nodes = c(name_baseline_covariates, sapply(time_grid, function(k) {paste0(c(name_time_covariates, name_competing_risk), "_", k)}))
  L_nodes = c(sapply(time_grid, function(k) {paste0(c(name_time_covariates), "_", k)}))
  L_nodes = L_nodes[match(L_nodes, names(work_data),nomatch = 0)!=0]
  if(length(name_censoring)==0) {C_nodes = NULL}
  list(data = work_data[],
       Anodes = intersect(A_nodes, names(work_data)),
       Cnodes = intersect(C_nodes, names(work_data)),
       Dnodes = intersect(D_nodes, names(work_data)),
       Lnodes = intersect(L_nodes, names(work_data)), 
       Ynodes = intersect(Y_nodes, names(work_data)),
       event_counts = event_counts)
}
