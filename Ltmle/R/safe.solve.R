safe.solve <-
function (a, b) 
{
    if (missing(b)) {
        try.result <- try(x <- solve(a))
    }
    else {
        try.result <- try(x <- solve(a, b))
    }
    if (inherits(try.result, "try-error")) {
        if (missing(b)) {
            x <- matrix(nrow = nrow(a), ncol = ncol(a))
        }
        else {
            x <- matrix(nrow = ncol(a), ncol = ncol(AsMatrix(b)))
        }
        warning("Error in solve(), standard errors not available")
    }
    return(x)
}
