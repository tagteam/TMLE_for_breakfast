Bound <-
function (x, bounds) 
{
    stopifnot(length(bounds) == 2 && !anyNA(bounds))
    x[x < min(bounds)] <- min(bounds)
    x[x > max(bounds)] <- max(bounds)
    return(x)
}
