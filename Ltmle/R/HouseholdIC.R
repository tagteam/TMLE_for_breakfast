HouseholdIC <-
function (recordIC, id) 
{
    if (is.null(id)) 
        return(recordIC)
    householdIC <- as.matrix(aggregate(recordIC, list(id = id), 
        sum)[, -1, drop = FALSE])
    num.records <- nrow(recordIC)
    num.households <- nrow(householdIC)
    householdIC <- householdIC * num.households/num.records
    return(householdIC)
}
