AsMatrix <-
function (x) 
{
    if (is.matrix(x)) {
        return(x)
    }
    else if (is.vector(x)) {
        dim(x) <- c(length(x), 1)
        return(x)
    }
    else {
        stop("AsMatrix input should be a matrix or vector")
    }
}
