IsUncensored <-
function (uncensored.matrix, Cnodes, cur.node) 
{
    index <- which.max(Cnodes[Cnodes < cur.node])
    if (length(index) == 0) 
        return(rep(TRUE, nrow(uncensored.matrix)))
    return(uncensored.matrix[, index])
}
