IsDeterministic <-
function (data, cur.node, deterministic.Q.function, nodes, called.from.estimate.g, 
    survivalOutcome) 
{
    if (survivalOutcome && any(nodes$Y < cur.node)) {
        last.Ynode <- max(nodes$Y[nodes$Y < cur.node])
        is.deterministic <- data[, last.Ynode] %in% TRUE
        ## Competing risks
        ## if (length(nodes$D)>0 && any(nodes$D<cur.node)){
            ## last.Dnode <- max(nodes$D[nodes$D < cur.node])
            ## is.deterministic <- is.deterministic|(data[,last.Dnode]%in%TRUE)
        ## }
    }
    else {
        is.deterministic <- rep(FALSE, nrow(data))
    }
    default <- list(is.deterministic = is.deterministic, Q.value = 1)
    if (is.null(deterministic.Q.function)) 
        return(default)
    det.list <- deterministic.Q.function(data = data, current.node = cur.node, 
        nodes = nodes, called.from.estimate.g = called.from.estimate.g)
    if (is.null(det.list)) 
        return(default)
    if (called.from.estimate.g) {
        if (!is.list(det.list) || !("is.deterministic" %in% names(det.list)) || 
            !(length(det.list) %in% 1:2)) 
            stop("deterministic.Q.function should return a list with names: is.deterministic, Q.value")
    }
    else {
        if (!is.list(det.list) || !setequal(names(det.list), 
            c("is.deterministic", "Q.value")) || length(det.list) != 
            2) 
            stop("deterministic.Q.function should return a list with names: is.deterministic, Q.value")
    }
    if (!length(det.list$Q.value) %in% c(1, length(which(det.list$is.deterministic)))) 
        stop("the length of the 'Q.value' element of deterministic.Q.function's return argument should be either 1 or length(which(det.list$is.deterministic))")
    Q.value.from.function <- rep(NA, nrow(data))
    Q.value.from.function[det.list$is.deterministic] <- det.list$Q.value
    set.by.function.and.death <- is.deterministic & det.list$is.deterministic
    if (any(Q.value.from.function[set.by.function.and.death] != 
        1)) {
        stop(paste("inconsistent deterministic Q at node:", names(data)[cur.node]))
    }
    finalY <- data[, max(nodes$Y)]
    inconsistent.rows <- (det.list$Q.value %in% c(0, 1)) & (det.list$Q.value != 
        finalY[det.list$is.deterministic]) & !is.na(finalY[det.list$is.deterministic])
    if (any(inconsistent.rows)) 
        stop(paste("At node:", names(data)[cur.node], "deterministic.Q.function is inconsistent with data - Q.value is either 0 or 1 but this does not match the final Y node value\nCheck data rows:", 
            paste(head(rownames(data)[det.list$is.deterministic][inconsistent.rows]), 
                collapse = " ")))
    Q.value <- rep(NA, nrow(data))
    Q.value[is.deterministic] <- 1
    Q.value[det.list$is.deterministic] <- det.list$Q.value
    is.deterministic <- is.deterministic | det.list$is.deterministic
    Q.value <- Q.value[is.deterministic]
    if (anyNA(is.deterministic) || anyNA(Q.value)) 
        stop("NA in is.deterministic or Q.value")
    return(list(is.deterministic = is.deterministic, Q.value = Q.value))
}
