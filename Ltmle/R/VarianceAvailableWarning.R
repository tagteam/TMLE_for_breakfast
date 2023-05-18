VarianceAvailableWarning <-
function (inputs) 
{
    if (!inputs$binaryOutcome) 
        return("Robust variance estimate is not currently available with non binary outcomes")
    if (!is.null(inputs$deterministic.Q.function)) 
        return("Robust variance estimate is not currently available with deterministic.Q.function")
    if (inputs$gcomp) 
        return("Robust variance estimate is not currently available with gcomp")
    if (inputs$stratify) 
        return("Robust variance estimate is not currently available with stratify=TRUE")
    if (!is.null(inputs$id)) 
        return("Robust variance estimate is not currently available with non-null id")
    return(NULL)
}
