ConvertCensoringNodes <-
function (data, Cnodes, has.deterministic.functions = FALSE) 
{
    error.msg <- "in data, all Cnodes should be factors with two levels, 'censored' and 'uncensored'\n See ?BinaryToCensoring \n (binary is also accepted, where 0=censored, 1=uncensored, but this is not recommended)"
    for (i in Cnodes) {
        col <- data[, i]
        if (is.numeric(col)) {
            if (!all(col %in% c(0, 1, NA))) 
                stop(error.msg)
            data[, i] <- BinaryToCensoring(is.uncensored = col)
            if (has.deterministic.functions) 
                warning("Censoring nodes have been converted from binaries to factors - see ?BinaryToCensoring.\n Note that if you are writing your own deterministic.g.function or deterministic.Q.function that censoring nodes are converted to factors\n before these functions are called.")
        }
        else if (is.factor(col)) {
            if (!all(levels(col) %in% c("censored", "uncensored"))) {
                stop("all levels of data[, Cnodes] should be in censored, uncensored (NA should not be a level)")
            }
        }
        else {
            stop(error.msg)
        }
    }
    return(data)
}
