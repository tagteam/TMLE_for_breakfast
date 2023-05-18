InterventionMatch <-
function (intervention.match.array, Anodes, cur.node) 
{
    index <- which.max(Anodes[Anodes < cur.node])
    if (length(index) == 0) 
        return(matrix(TRUE, nrow(intervention.match.array), dim(intervention.match.array)[3]))
    return(AsMatrix(intervention.match.array[, index, ]))
}
