## Adjust data to fit ltmle constraints
get_ltmle_data <- function(work_data, time_horizon,
                           name_outcome,
                           name_baseline_covariates,
                           name_time_covariates,
                           name_regimen,
                           name_censoring = NULL,
                           censored_label = "1",
                           name_comp.event = NULL,
                           order_YC = TRUE){
  time_grid = 0:time_horizon
  K = length(time_grid)
  
  if(length(name_censoring)>0){
      for(col in sapply(time_grid[-c(1,K)], function(timepoint){paste0(name_censoring,"_",timepoint)})){
          set(work_data, j = col, value=ifelse(work_data[[col]]==censored_label,"censored","uncensored"))
          set(work_data, j = col, value=as.factor(work_data[[col]]))
      }
      if (order_YC == FALSE){
          set(work_data, j = time_grid[K], value=ifelse(work_data[[col]]==censored_label,"censored","uncensored"))
          set(work_data, j = time_grid[K], value=as.factor(work_data[[col]]))
      }
  }
  
  ## Manipulation of the event nodes
  if(length(name_regimen)==2){
    A_nodes = c(t(matrix(c(paste0(name_regimen[[1]],"_",time_grid[-K]),paste0(name_regimen[[2]],"_",time_grid[-K])), nrow = K-1)))
  } else {
    A_nodes = paste0(name_regimen[[1]],"_",time_grid[-K])
  }

  Y_nodes = paste0(name_outcome,"_",time_grid[-1])
  D_nodes = paste0(name_comp.event,"_",time_grid[-c(1,K)])
  if (order_YC == TRUE){
      # the node C_K is after Y_K and was removed
      C_nodes = paste0(name_censoring,"_",time_grid[-c(1,K)])
  } else{
      # the node C_1 is before Y_1 and removed
      C_nodes = paste0(name_censoring,"_",time_grid[-1])
  }
  A_nodes_position = match(A_nodes,names(work_data))
  Y_nodes_position = match(Y_nodes,names(work_data))
  D_nodes_position = match(D_nodes,names(work_data))
  C_nodes_position = match(C_nodes,names(work_data))

  ## Adjust data depending on censoring/event/competing event with NA
  
  for(q in 1:(K-1)){
      if(q<(K-1)){
          has_outcome_or_death_and_censored = (((work_data[[Y_nodes_position[[q]]]]%in%1)|(work_data[[D_nodes_position[[q]]]]%in%1))&
                                               (work_data[[C_nodes_position[[q]]]]%in%"censored"))
          if(any(has_outcome_or_death_and_censored)){
              set(work_data,j=C_nodes_position[[q]],i=which(has_outcome_or_death_and_censored),value="uncensored")
          }
      } else{ # last time point
          if (order_YC == FALSE){
              has_outcome_or_death_and_censored = ((work_data[[Y_nodes_position[[q]]]]%in%1)&(work_data[[C_nodes_position[[q]]]]%in%"censored"))
              if(any(has_outcome_or_death_and_censored)){
                  set(work_data,j=C_nodes_position[[q]],i=which(has_outcome_or_death_and_censored),value="uncensored")
              }
          }
      }
  }
  ## All nodes (except outcome and competing risk) should be NA after an event (outcome or death)
  
  if(time_horizon!= 1){
      if(order_YC == TRUE){
          for(k in Y_nodes_position){
              later_nodes=setdiff((k+1):NCOL(work_data),Y_nodes_position)
              later_nodes = later_nodes[later_nodes <= NCOL(work_data)]
              later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
              if(any(has_outcome <- (work_data[[k]]%in%1))){
                  for(l in later_nodes) {set(work_data,j=l,i=which(has_outcome),value=NA)}
                  for(l in later_Y_nodes) {set(work_data,j=l,i=which(has_outcome),value=1)}
              }
          }
      
          if(length(name_comp.event)>0){
              for(k in D_nodes_position){
                  later_nodes=setdiff((k+1):NCOL(work_data),Y_nodes_position)
                  later_nodes = later_nodes[later_nodes <= NCOL(work_data)]
                  # later_D_nodes=intersect((k+1):D_nodes_position[length(D_nodes_position)],D_nodes_position)
                  if(any(has_died <- (work_data[[k]]%in%1))){
                      for(l in later_nodes) {set(work_data,j=l,i=which(has_died),value=NA)}
                      # for(l in later_D_nodes) {set(work_data,j=l,i=which(has_died),value=1)}
                  }
              }
          }
      
      ## All nodes should be NA as soon as censoring has occurred
      
          if(length(name_censoring)>0){
              for(k in C_nodes_position[-(K-1)]){
                  later_nodes=(k+1):NCOL(work_data)
                  if(any(has_censored <- (work_data[[k]]%in%"censored"))){
                      for(l in later_nodes) {set(work_data,j=l,i=which(has_censored),value=NA)}
                  }
              }
          }
      } else {
          for(k in Y_nodes_position[-(K-1)]){
              later_nodes=setdiff((k+1):NCOL(work_data),Y_nodes_position)
              later_nodes = later_nodes[later_nodes <= NCOL(work_data)]
              later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
              if(any(has_outcome <- (work_data[[k]]%in%1))){
                  for(l in later_nodes) {set(work_data,j=l,i=which(has_outcome),value=NA)}
                  for(l in later_Y_nodes) {set(work_data,j=l,i=which(has_outcome),value=1)}
              }
          }
      
          if(length(name_comp.event)>0){
              for(k in D_nodes_position){
                  later_nodes=setdiff((k+1):NCOL(work_data),Y_nodes_position)
                  later_nodes = later_nodes[later_nodes <= NCOL(work_data)]
                  # later_D_nodes=intersect((k+1):D_nodes_position[length(D_nodes_position)],D_nodes_position)
                  if(any(has_died <- (work_data[[k]]%in%1))){
                      for(l in later_nodes) {set(work_data,j=l,i=which(has_died),value=NA)}
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
    }
  }
  
  
  # L_nodes = c(name_baseline_covariates, sapply(time_grid, function(k) {paste0(c(name_time_covariates, name_comp.event), "_", k)}))
  L_nodes = c(sapply(time_grid, function(k) {paste0(c(name_time_covariates, name_comp.event), "_", k)}))
  L_nodes = L_nodes[match(L_nodes, names(work_data),nomatch = 0)!=0]
  
  if(length(name_censoring)==0) {C_nodes = NULL}
  
  list(data = work_data[],
       Anodes = A_nodes,
       Cnodes = C_nodes,
       Lnodes = L_nodes, 
       Ynodes = Y_nodes)
}
