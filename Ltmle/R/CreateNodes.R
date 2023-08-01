CreateNodes <-
function (data, Anodes, Cnodes, Dnodes, Lnodes, Ynodes) 
{
    Anodes <- NodeToIndex(data, Anodes)
    Cnodes <- NodeToIndex(data, Cnodes)
    Dnodes <- NodeToIndex(data, Dnodes)
    Lnodes <- NodeToIndex(data, Lnodes)
    Ynodes <- NodeToIndex(data, Ynodes)
    nodes <- SuppressGivenWarnings(list(A = Anodes, C = Cnodes, D = Dnodes,
                                        L = Lnodes, Y = Ynodes, AC = sort(c(Anodes, Cnodes))), 
        "is.na() applied to non-(list or vector) of type 'NULL'")
    nodes$baseline <- sseq(1, min(c(nodes$A, nodes$L, nodes$C, nodes$D,
        nodes$Y)) - 1)
    nodes$LY <- CreateLYNodes(data, nodes, check.Qform = FALSE)
    return(nodes)
}
