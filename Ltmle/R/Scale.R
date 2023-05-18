Scale <-
function (x, min.y, max.y) 
{
    if (all(is.na(x))) 
        stop("all NA in Scale")
    r <- range(x, na.rm = TRUE)
    if (diff(r) > 0) {
        return((x - r[1])/diff(r) * (max.y - min.y) + min.y)
    }
    else {
        if (r[1] >= min.y && r[1] <= max.y) {
            return(rep(r[1], length(x)))
        }
        else {
            return(rep(mean(c(min.y, max.y)), length(x)))
        }
    }
}
