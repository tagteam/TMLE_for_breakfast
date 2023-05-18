CheckData <-
function (data) 
{
    if (inherits(data, "data.frame")) {
        return(droplevels(as.data.frame(data)))
    }
    stop("data must be a data frame (or inherit data.frame)")
}
