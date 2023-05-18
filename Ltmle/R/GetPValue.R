GetPValue <-
function (q, n) 
{
    if (n < 100) {
        return(pt(q, n - 1))
    }
    else {
        return(pnorm(q))
    }
}
