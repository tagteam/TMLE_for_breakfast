CalcInterventionMatchArray <-
function (data, regimes, Anodes) 
{
    num.regimes <- dim(regimes)[3]
    intervention.match <- array(dim = c(nrow(data), length(Anodes), 
        num.regimes))
    cum.intervention.match <- matrix(TRUE, nrow(data), num.regimes)
    for (Anode.index in seq_along(Anodes)) {
        cum.intervention.match <- cum.intervention.match & ((data[, 
            Anodes[Anode.index]] == regimes[, Anode.index, ]) %in% 
            c(TRUE, NA))
        intervention.match[, Anode.index, ] <- cum.intervention.match
    }
    return(intervention.match)
}
