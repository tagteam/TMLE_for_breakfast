repmat <-
function (X, m, n) 
{
    mx <- dim(X)[1]
    nx <- dim(X)[2]
    if ((m == 0) || (n == 0)) 
        return(matrix(numeric(0), nrow = mx * m, ncol = nx * 
            n))
    return(matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = TRUE))
}
