event_node_manipulator <- function(data,k,outcome,competing="Dead",censored="Censored",outcome_is_competing="death"){
    #
    # manipulation of the event nodes
    #
    Y_nodes <- paste0(outcome,"_",1:k)
    D_nodes <- paste0(competing,"_",1:k)
    C_nodes <- paste0(censored,"_",1:k)
    Y_nodes_position <- match(Y_nodes,names(data))
    C_nodes_position <- match(C_nodes,names(data))
    D_nodes_position <- match(D_nodes,names(data))
    #
    # IMPORTANT: when outcome or death occurs in an interval followed by censoring,
    # we choose to uncensor the outcome and the death.
    #
    for (q in 1:k){
        if (q<k){
            has_outcome_or_death_and_censored <- (((data[[Y_nodes_position[[q]]]]%in%1)|(data[[D_nodes_position[[q]]]]%in%1)) &(data[[C_nodes_position[[q]]]]%in%"censored"))
        } else{
            has_outcome_or_death_and_censored <- ((data[[Y_nodes_position[[q]]]]%in%1)&(data[[C_nodes_position[[q]]]]%in%"censored"))
        }
        if (any(has_outcome_or_death_and_censored)){
            set(data,j=C_nodes_position[[q]],i=which(has_outcome_or_death_and_censored),value="uncensored")
        }
    }
    #
    # all nodes (but not outcome and competing risk nodes) should be NA after outcome
    # or competing risk (i.e., death) has occurred
    #
    for (Ok in Y_nodes[-k]){
        later_nodes=setdiff((match(Ok,names(data))+1):NCOL(data),Y_nodes_position)
        if (any(has_outcome <- (data[[Ok]]%in%1))){
            for (l in later_nodes) set(data,j=l,i=which(has_outcome),value=NA)
        }
    }
    if (outcome!=outcome_is_competing){
        # the last Death node occurs after outcome in the last interval and has been removed
        for (Dk in D_nodes[-k]){
            # for some reason Y_nodes must not be missing after death!
            later_nodes=setdiff((match(Dk,names(data))+1):NCOL(data),Y_nodes_position)
            if (any(has_died <- (data[[Dk]]%in%1))){
                for (l in later_nodes) set(data,j=l,i=which(has_died),value=NA)
            }
        }
    }
    #
    # all nodes including outcome should be NA as soon as censored occurred
    #
    for (Ck in C_nodes){
        ## later_nodes=setdiff((match(Ck,names(data))+1):NCOL(data),c(Y_nodes,D_nodes))
        later_nodes=(match(Ck,names(data))+1):NCOL(data)
        if (any(has_censored <- (data[[Ck]]%in%"censored"))){
            for (l in later_nodes) set(data,j=l,i=which(has_censored),value=NA)
        }
    }
    data
}
