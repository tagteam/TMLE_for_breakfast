RegimesFromAbar <-
function (data, abar, rule) 
{
    if (!is.null(rule)) {
        if (!(missing(abar) || is.null(abar))) 
            stop("'abar' should not be specified when using a 'rule' function")
        rule.output.length <- length(as.numeric(rule(data[1, 
            ])))
        if (all(sapply(data, is.numeric))) {
            abar <- apply(data, 1, rule)
            if (rule.output.length == 1) {
                abar <- AsMatrix(abar)
            }
            else {
                abar <- t(abar)
            }
        }
        else {
            if (nrow(data) > 10000) 
                warning("Using a rule input may be significantly slower than using abar/regimes if your data contains censoring nodes or is otherwise not all numeric.")
            abar <- matrix(nrow = nrow(data), ncol = rule.output.length)
            for (i in seq_len(nrow(data))) {
                abar[i, ] <- as.numeric(rule(data[i, ]))
            }
        }
    }
    if (is.vector(abar)) {
        abar <- matrix(rep(abar, each = nrow(data)), nrow = nrow(data))
    }
    else if (is.null(abar)) {
        abar <- matrix(numeric(0), nrow = nrow(data), ncol = 0)
    }
    else if (is.function(abar)) {
        stop("abar should be a vector or matrix, not a function. Use the 'rule' paramter to pass a function instead.")
    }
    regimes <- abar
    dim(regimes) <- c(nrow(regimes), ncol(regimes), 1)
    return(regimes)
}
