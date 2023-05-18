get_sim_data <- function(lava_model, sample_size){
  simdata = as.data.table(sim(lava_model$model, sample_size))
  simdata[, pnr := c(1:simdata[,.N])] ## Include pnr in simdata
  
  Y_nodes_position = match(paste0(lava_model$outcome,"_",1:lava_model$time_horizon), names(simdata))
  C_nodes_position = match(paste0(lava_model$censoring,"_",1:lava_model$time_horizon), names(simdata))
  D_nodes_position = match(paste0(lava_model$comp.event,"_",1:(lava_model$time_horizon-1)), names(simdata))
  
  if(lava_model$order_YC){
    ## Make sure that no censoring or death happens after event
    for(k in Y_nodes_position){
      later_D_nodes=ifelse(length(lava_model$comp.event)>0, intersect((k+1):D_nodes_position[length(D_nodes_position)],D_nodes_position), NA)
      later_C_nodes=ifelse(length(lava_model$censoring)>0, intersect((k+1):C_nodes_position[length(C_nodes_position)],C_nodes_position), NA)
      later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
      if(any(has_outcome <- (simdata[[k]]%in%1))){
        if(length(lava_model$comp.event)>0){for(l in later_D_nodes) {set(simdata,j=l,i=which(has_outcome),value=0)}}
        if(length(lava_model$censoring)>0){for(l in later_C_nodes) {set(simdata,j=l,i=which(has_outcome),value=1)}} ## Because 1 means uncensored
        for(l in later_Y_nodes) {set(simdata,j=l,i=which(has_outcome),value=1)}
      }
    }
    ## Make sure that no event or death happens after censoring
    if(length(lava_model$censoring)>0){ ## Make sure that no event happens after death
      for(k in C_nodes_position){
        later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
        later_D_nodes=ifelse(length(lava_model$comp.event)>0, intersect((k+1):D_nodes_position[length(D_nodes_position)],D_nodes_position), NA)
        later_C_nodes=intersect((k+1):C_nodes_position[length(C_nodes_position)],C_nodes_position)
        if(any(is_censored <- (simdata[[k]]%in%0))){
          for(l in later_Y_nodes) {set(simdata,j=l,i=which(is_censored),value=0)}
          if(length(lava_model$comp.event)>0){for(l in later_D_nodes) {set(simdata,j=l,i=which(is_censored),value=0)}}
          for(l in later_C_nodes) {set(simdata,j=l,i=which(is_censored),value=0)}
        }
      }
    }
    ## Make sure that no event or censoring happens after death
    if(length(lava_model$comp.event)>0){
      for(k in D_nodes_position){
        later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
        later_C_nodes=ifelse(length(lava_model$censoring)>0,intersect((k+1):C_nodes_position[length(C_nodes_position)],C_nodes_position), NA)
        later_D_nodes=intersect((k+1):D_nodes_position[length(D_nodes_position)],D_nodes_position)
        if(any(has_died <- (simdata[[k]]%in%1))){
          for(l in later_Y_nodes) {set(simdata,j=l,i=which(has_died),value=0)}
          if(length(lava_model$censoring)>0) {for(l in later_C_nodes) {set(simdata,j=l,i=which(has_died),value=1)}} ## Because 1 means uncensored
          for(l in later_D_nodes) {set(simdata,j=l,i=which(has_died),value=1)}
        }
      }
    }
  } else {
    ## Make sure that no event or death happens after censoring
    if(length(lava_model$censoring)>0){ ## Make sure that no event happens after death
      for(k in C_nodes_position){
        later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
        later_D_nodes=ifelse(length(lava_model$comp.event)>0, intersect((k+1):D_nodes_position[length(D_nodes_position)],D_nodes_position), NA)
        later_C_nodes=intersect((k+1):C_nodes_position[length(C_nodes_position)],C_nodes_position)
        if(any(is_censored <- (simdata[[k]]%in%0))){
          for(l in later_Y_nodes) {set(simdata,j=l,i=which(is_censored),value=0)}
          if(length(lava_model$comp.event)>0){for(l in later_D_nodes) {set(simdata,j=l,i=which(is_censored),value=0)}}
          for(l in later_C_nodes) {set(simdata,j=l,i=which(is_censored),value=0)}
        }
      }
    }
    ## Make sure that no censoring or death happens after event
    for(k in Y_nodes_position){
      later_D_nodes=ifelse(length(lava_model$comp.event)>0, intersect((k+1):D_nodes_position[length(D_nodes_position)],D_nodes_position), NA)
      later_C_nodes=ifelse(length(lava_model$censoring)>0, intersect((k+1):C_nodes_position[length(C_nodes_position)],C_nodes_position), NA)
      later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
      if(any(has_outcome <- (simdata[[k]]%in%1))){
        if(length(lava_model$comp.event)>0){for(l in later_D_nodes) {set(simdata,j=l,i=which(has_outcome),value=0)}}
        if(length(lava_model$censoring)>0){for(l in later_C_nodes) {set(simdata,j=l,i=which(has_outcome),value=1)}} ## Because 1 means uncensored
        for(l in later_Y_nodes) {set(simdata,j=l,i=which(has_outcome),value=1)}
      }
    }
    ## Make sure that no event or censoring happens after death
    if(length(lava_model$comp.event)>0){
      for(k in D_nodes_position){
        later_Y_nodes=intersect((k+1):Y_nodes_position[length(Y_nodes_position)],Y_nodes_position)
        later_C_nodes=ifelse(length(lava_model$censoring)>0,intersect((k+1):C_nodes_position[length(C_nodes_position)],C_nodes_position), NA)
        later_D_nodes=intersect((k+1):D_nodes_position[length(D_nodes_position)],D_nodes_position)
        if(any(has_died <- (simdata[[k]]%in%1))){
          for(l in later_Y_nodes) {set(simdata,j=l,i=which(has_died),value=0)}
          if(length(lava_model$censoring)>0) {for(l in later_C_nodes) {set(simdata,j=l,i=which(has_died),value=1)}} ## Because 1 means uncensored
          for(l in later_D_nodes) {set(simdata,j=l,i=which(has_died),value=1)}
        }
      }
    }
  }
  
  simdata[]
}