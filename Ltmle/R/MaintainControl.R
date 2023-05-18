MaintainControl <-
function (data, current.node, nodes) 
{
    Anodes <- nodes$A
    if (!(current.node %in% Anodes)) 
        return(NULL)
    if (!(any(Anodes < current.node))) 
        return(NULL)
    prev.a.node <- max(Anodes[Anodes < current.node])
    is.deterministic <- data[, prev.a.node] == 0
    return(list(is.deterministic = is.deterministic, prob1 = 0))
}
