SetA <-
function (data, regimes, Anodes, regime.index, cur.node) 
{
    Anode.index <- which(Anodes < cur.node)
    for (a in Anode.index) {
        data[, Anodes[a]] <- regimes[, a, regime.index]
    }
    return(data)
}
