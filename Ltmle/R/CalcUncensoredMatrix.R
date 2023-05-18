CalcUncensoredMatrix <-
function (data, Cnodes) 
{
    uncensored <- matrix(nrow = nrow(data), ncol = length(Cnodes))
    cum.uncensored <- rep(TRUE, nrow(data))
    for (Cnode.index in seq_along(Cnodes)) {
        cum.uncensored <- cum.uncensored & (data[, Cnodes[Cnode.index]] %in% 
            c("uncensored", NA))
        uncensored[, Cnode.index] <- cum.uncensored
    }
    return(uncensored)
}
