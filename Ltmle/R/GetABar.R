GetABar <-
function (regimes, regime.index, Anodes) 
{
    abar <- AsMatrix(regimes[, seq_along(Anodes), regime.index])
    return(abar)
}
