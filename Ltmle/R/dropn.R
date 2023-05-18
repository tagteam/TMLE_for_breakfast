dropn <-
function (x, n) 
{
    stopifnot(length(dim(x)) == n)
    stopifnot(dim(x)[n] == 1)
    dn <- dimnames(x)
    dim(x) <- dim(x)[1:(n - 1)]
    dimnames(x) <- dn[1:(n - 1)]
    return(x)
}
